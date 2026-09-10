/*
 * Copyright 2026 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.server.commands;

import de.markusbordihn.easynpc.commands.Command;
import de.markusbordihn.easynpc.config.SecurityConfig;
import de.markusbordihn.easynpc.data.preset.PresetData;
import de.markusbordihn.easynpc.data.preset.PresetExportFormat;
import de.markusbordihn.easynpc.data.preset.PresetType;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.handler.ImportOutcome;
import de.markusbordihn.easynpc.handler.PlacementHandler;
import de.markusbordihn.easynpc.handler.PresetFeedback;
import de.markusbordihn.easynpc.handler.PresetHandler;
import de.markusbordihn.easynpc.handler.PresetImportResult;
import de.markusbordihn.easynpc.io.CustomPresetDataFiles;
import de.markusbordihn.easynpc.io.DataPresetDataFiles;
import de.markusbordihn.easynpc.io.DefaultPresetDataFiles;
import de.markusbordihn.easynpc.io.WorldPresetDataFiles;
import de.markusbordihn.easynpc.security.ActorSecurityContext;
import de.markusbordihn.easynpc.security.CommandSecurity;
import de.markusbordihn.easynpc.security.FeatureSecurity;
import de.markusbordihn.easynpc.security.NpcFeature;
import de.markusbordihn.easynpc.security.SpawnRateLimiter;
import de.markusbordihn.easynpc.utils.ResourceNameNormalizer;
import java.util.Comparator;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Pattern;
import java.util.stream.Stream;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.phys.Vec3;

class PresetBatchImportCommand extends Command {

  private static final Map<UUID, Long> lastBatchImport = new ConcurrentHashMap<>();

  private PresetBatchImportCommand() {}

  static int importMatching(
      CommandSourceStack context,
      PresetType presetType,
      String pattern,
      Vec3 position,
      boolean keepIdentity,
      boolean confirmed) {
    Pattern presetPattern = ResourceNameNormalizer.toPresetPathPattern(pattern);
    if (presetPattern == null) {
      return sendFailureMessage(context, PresetFeedback.batchPatternInvalid(pattern));
    }

    ActorSecurityContext actorSecurityContext = CommandSecurity.getActorContext(context);
    ServerPlayer serverPlayer = actorSecurityContext != null ? actorSecurityContext.player() : null;
    if (serverPlayer != null) {
      if (!FeatureSecurity.checkActorFeatureAccess(serverPlayer, NpcFeature.SPAWN_NPC).allowed()) {
        return sendFailureMessage(context, PresetFeedback.spawnDenied());
      }
      if (keepIdentity
          && !FeatureSecurity.checkActorFeatureAccess(serverPlayer, NpcFeature.POSITION)
              .allowed()) {
        return sendFailureMessage(context, PresetFeedback.restoreDenied());
      }
    }

    List<ResourceLocation> matches =
        findMatches(context.getServer(), presetType, presetPattern, pattern.contains("/"));
    if (matches.isEmpty()) {
      return sendFailureMessage(context, PresetFeedback.batchNoMatches(pattern));
    }
    if (matches.size() > SecurityConfig.NPC_PRESET_BATCH_LIMIT) {
      return sendFailureMessage(
          context,
          PresetFeedback.batchTooMany(matches.size(), SecurityConfig.NPC_PRESET_BATCH_LIMIT));
    }

    long remainingCooldown = remainingCooldown(serverPlayer);
    if (remainingCooldown > 0) {
      return sendFailureMessage(context, PresetFeedback.batchCooldown(remainingCooldown));
    }

    int existingEntities =
        keepIdentity ? countExistingEntities(context.getLevel(), presetType, matches) : 0;
    if (!confirmed
        && (existingEntities > 0
            || matches.size() > SecurityConfig.NPC_PRESET_BATCH_CONFIRM_THRESHOLD)) {
      for (Component message :
          PresetFeedback.batchPreview(matches, existingEntities, keepIdentity)) {
        sendSuccessMessage(context, message);
      }

      return Command.SINGLE_SUCCESS;
    }

    List<ResourceLocation> allowedMatches = applyRateLimit(context, serverPlayer, matches);
    if (allowedMatches.isEmpty()) {
      return Command.FAILURE;
    }

    return runBatch(
        context,
        actorSecurityContext,
        serverPlayer,
        presetType,
        allowedMatches,
        position,
        keepIdentity);
  }

  private static int runBatch(
      CommandSourceStack context,
      ActorSecurityContext actorSecurityContext,
      ServerPlayer serverPlayer,
      PresetType presetType,
      List<ResourceLocation> matches,
      Vec3 position,
      boolean keepIdentity) {
    ServerLevel serverLevel = context.getLevel();
    EnumMap<ImportOutcome, Integer> outcomes = new EnumMap<>(ImportOutcome.class);
    for (ResourceLocation preset : matches) {
      Vec3 importPosition = null;
      UUID importUUID = null;
      if (!keepIdentity) {
        importPosition = PlacementHandler.findFreePositionNear(serverLevel, position);
        importUUID = UUID.randomUUID();
      }

      PresetImportResult importResult =
          PresetHandler.importPresetWithReport(
              serverLevel,
              presetType,
              preset,
              importPosition,
              importUUID,
              actorSecurityContext,
              serverPlayer);
      ImportOutcome outcome = importResult.outcome();
      outcomes.merge(outcome != null ? outcome : ImportOutcome.FAILED, 1, Integer::sum);
    }
    if (serverPlayer != null) {
      lastBatchImport.put(serverPlayer.getUUID(), System.currentTimeMillis());
    }

    return sendSuccessMessage(
        context,
        PresetFeedback.batchResult(
            outcomes.getOrDefault(ImportOutcome.CREATED, 0),
            outcomes.getOrDefault(ImportOutcome.UPDATED_EXISTING, 0),
            outcomes.getOrDefault(ImportOutcome.REPLACED_EXISTING, 0),
            outcomes.getOrDefault(ImportOutcome.FAILED, 0)));
  }

  private static List<ResourceLocation> applyRateLimit(
      CommandSourceStack context, ServerPlayer serverPlayer, List<ResourceLocation> matches) {
    if (serverPlayer == null) {
      return matches;
    }

    int grantedSpawns = SpawnRateLimiter.checkAndRecord(serverPlayer, matches.size());
    if (grantedSpawns < matches.size()) {
      sendFailureMessage(
          context, PresetFeedback.spawnRateLimited(SpawnRateLimiter.spawnLimit(serverPlayer)));
    }
    if (grantedSpawns <= 0) {
      return List.of();
    }

    return matches.subList(0, grantedSpawns);
  }

  private static long remainingCooldown(ServerPlayer serverPlayer) {
    if (serverPlayer == null || SecurityConfig.NPC_PRESET_BATCH_COOLDOWN <= 0) {
      return 0;
    }

    Long lastImport = lastBatchImport.get(serverPlayer.getUUID());
    if (lastImport == null) {
      return 0;
    }

    long elapsedSeconds = (System.currentTimeMillis() - lastImport) / 1000L;
    return Math.max(0, SecurityConfig.NPC_PRESET_BATCH_COOLDOWN - elapsedSeconds);
  }

  private static int countExistingEntities(
      ServerLevel serverLevel, PresetType presetType, List<ResourceLocation> matches) {
    int existingEntities = 0;
    for (ResourceLocation preset : matches) {
      PresetData presetData = PresetHandler.loadPreset(preset, presetType, serverLevel.getServer());
      if (presetData == null || presetData.getEntityUUID() == null) {
        continue;
      }
      if (LivingEntityManager.getServerEasyNPCEntityByUUID(presetData.getEntityUUID(), serverLevel)
          != null) {
        existingEntities++;
      }
    }

    return existingEntities;
  }

  private static List<ResourceLocation> findMatches(
      MinecraftServer server, PresetType presetType, Pattern pattern, boolean matchFullPath) {
    Stream<ResourceLocation> presets =
        switch (presetType) {
          case CUSTOM -> CustomPresetDataFiles.getPresetResourceLocations();
          case WORLD -> WorldPresetDataFiles.getPresetResourceLocations();
          case DATA -> DataPresetDataFiles.getUsablePresetResourceLocations(server);
          case DEFAULT -> DefaultPresetDataFiles.getPresetResourceLocations(server);
          default -> Stream.empty();
        };

    return presets
        .filter(preset -> pattern.matcher(matchName(preset, matchFullPath)).matches())
        .sorted(Comparator.comparing(ResourceLocation::toString))
        .toList();
  }

  private static String matchName(ResourceLocation preset, boolean matchFullPath) {
    String path = PresetExportFormat.removePresetExtension(preset.getPath());
    if (matchFullPath) {
      return path;
    }

    int separatorIndex = path.lastIndexOf('/');
    if (separatorIndex < 0) {
      return path;
    }

    return path.substring(separatorIndex + 1);
  }
}

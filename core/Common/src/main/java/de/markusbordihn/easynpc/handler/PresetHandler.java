/*
 * Copyright 2023 Markus Bordihn
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

package de.markusbordihn.easynpc.handler;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.preset.PresetAccess;
import de.markusbordihn.easynpc.data.preset.PresetCompactor;
import de.markusbordihn.easynpc.data.preset.PresetData;
import de.markusbordihn.easynpc.data.preset.PresetDataUtils;
import de.markusbordihn.easynpc.data.preset.PresetExportFormat;
import de.markusbordihn.easynpc.data.preset.PresetInheritance;
import de.markusbordihn.easynpc.data.preset.PresetMetadata;
import de.markusbordihn.easynpc.data.preset.PresetNormalizer;
import de.markusbordihn.easynpc.data.preset.PresetReference;
import de.markusbordihn.easynpc.data.preset.PresetType;
import de.markusbordihn.easynpc.data.skin.SkinModel;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.NavigationDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.PresetDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.VariantDataCapable;
import de.markusbordihn.easynpc.io.CustomPresetDataFiles;
import de.markusbordihn.easynpc.io.PresetFileHandler;
import de.markusbordihn.easynpc.io.WorldPresetDataFiles;
import de.markusbordihn.easynpc.security.ActorSecurityContext;
import de.markusbordihn.easynpc.security.CommandSecurity;
import de.markusbordihn.easynpc.security.PresetSanitizationResult;
import de.markusbordihn.easynpc.security.PresetWarningMessages;
import de.markusbordihn.easynpc.security.SecurityDecision;
import de.markusbordihn.easynpc.security.SecurityManager;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.Optional;
import java.util.UUID;
import java.util.function.BiFunction;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtAccounter;
import net.minecraft.nbt.NbtIo;
import net.minecraft.nbt.TagParser;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.phys.Vec3;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class PresetHandler {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private PresetHandler() {}

  public static boolean importPreset(
      ServerLevel serverLevel,
      PresetType presetType,
      ResourceLocation presetLocation,
      Vec3 position,
      UUID uuid,
      ServerPlayer serverPlayer) {
    return importPreset(
        serverLevel,
        presetType,
        presetLocation,
        position,
        uuid,
        CommandSecurity.getActorContext(serverPlayer),
        serverPlayer);
  }

  public static boolean importPreset(
      ServerLevel serverLevel,
      PresetType presetType,
      ResourceLocation presetLocation,
      Vec3 position,
      UUID uuid,
      ActorSecurityContext actorSecurityContext,
      ServerPlayer owner) {
    PresetData presetData =
        loadPresetFromSource(presetType, presetLocation, serverLevel.getServer());
    return presetData != null
        && isAccessAllowed(presetData, actorSecurityContext)
        && importPreset(serverLevel, presetData, position, uuid, actorSecurityContext, owner);
  }

  private static boolean isAccessAllowed(
      PresetData presetData, ActorSecurityContext actorSecurityContext) {
    if (actorSecurityContext == null || actorSecurityContext.player() == null) {
      return true;
    }

    PresetAccess presetAccess = presetData.metadata().access();
    if (presetAccess.isUsableByCommand()) {
      return true;
    }

    log.warn(
        "Blocked {} preset {} for player {}",
        presetAccess,
        presetData.location(),
        actorSecurityContext.player().getName().getString());
    return false;
  }

  public static boolean importPreset(
      ServerLevel serverLevel,
      PresetData presetData,
      Vec3 position,
      UUID uuid,
      ServerPlayer serverPlayer) {
    return importPreset(
        serverLevel,
        presetData,
        position,
        uuid,
        CommandSecurity.getActorContext(serverPlayer),
        serverPlayer);
  }

  public static boolean importPreset(
      ServerLevel serverLevel,
      PresetData presetData,
      Vec3 position,
      UUID uuid,
      ActorSecurityContext actorSecurityContext,
      ServerPlayer owner) {
    return importPresetAndGetEntity(
            serverLevel, presetData, position, uuid, actorSecurityContext, owner)
        .isPresent();
  }

  public static Optional<EasyNPC<?>> importPresetAndGetEntity(
      ServerLevel serverLevel,
      PresetData presetData,
      Vec3 position,
      UUID uuid,
      ActorSecurityContext actorSecurityContext,
      ServerPlayer owner) {
    if (presetData == null || !presetData.hasValidData()) {
      log.error("[{}] Invalid preset data for import", serverLevel);
      return Optional.empty();
    }

    PresetData updatedPresetData = presetData;
    if (position != null) {
      updatedPresetData = updatedPresetData.withPosition(position);
    }

    // Exports strip the UUID, so without one the entity could be spawned but never found again.
    UUID entityUUID = uuid;
    if (entityUUID == null && presetData.data().hasUUID(Entity.UUID_TAG)) {
      entityUUID = presetData.data().getUUID(Entity.UUID_TAG);
    }
    if (entityUUID == null) {
      entityUUID = UUID.randomUUID();
    }
    updatedPresetData = updatedPresetData.withUUID(entityUUID);

    PresetSanitizationResult sanitizationResult =
        SecurityManager.sanitizePresetImport(
            serverLevel,
            updatedPresetData.data(),
            updatedPresetData.presetType(),
            uuid,
            actorSecurityContext,
            owner != null ? owner.getUUID() : null);
    updatedPresetData =
        PresetData.create(
            updatedPresetData.name(),
            updatedPresetData.entityType(),
            sanitizationResult.sanitizedTag(),
            updatedPresetData.location(),
            updatedPresetData.presetType(),
            updatedPresetData.metadata());

    if (!importPresetData(serverLevel, updatedPresetData.data())) {
      return Optional.empty();
    }

    EasyNPC<?> easyNPC = LivingEntityManager.getServerEasyNPCEntityByUUID(entityUUID, serverLevel);
    if (easyNPC == null) {
      log.error("[{}] Error importing preset, no entity found for {}", serverLevel, entityUUID);
      return Optional.empty();
    }

    configureImportedEntity(easyNPC, position, owner);
    sendImportSanitizationWarnings(
        actorSecurityContext != null ? actorSecurityContext.player() : null, sanitizationResult);

    return Optional.of(easyNPC);
  }

  public static Optional<EasyNPC<?>> importPresetAndGetEntity(
      ServerLevel serverLevel,
      PresetType presetType,
      ResourceLocation presetLocation,
      Vec3 position,
      UUID uuid,
      ActorSecurityContext actorSecurityContext,
      ServerPlayer owner) {
    PresetData presetData =
        loadPresetFromSource(presetType, presetLocation, serverLevel.getServer());
    if (presetData == null || !isAccessAllowed(presetData, actorSecurityContext)) {
      return Optional.empty();
    }

    return importPresetAndGetEntity(
        serverLevel, presetData, position, uuid, actorSecurityContext, owner);
  }

  private static void sendImportSanitizationWarnings(
      ServerPlayer serverPlayer, PresetSanitizationResult sanitizationResult) {
    if (serverPlayer == null
        || sanitizationResult == null
        || !sanitizationResult.changed()
        || sanitizationResult.notices().isEmpty()) {
      return;
    }

    var playerMessages = PresetWarningMessages.toPlayerMessages(sanitizationResult);
    if (playerMessages.isEmpty()) {
      return;
    }

    serverPlayer.sendSystemMessage(
        Component.literal("Preset imported, but some features were limited by server rules."));
    int shownNotices = 0;
    for (String message : playerMessages) {
      if (shownNotices >= 3) {
        break;
      }
      serverPlayer.sendSystemMessage(Component.literal("- " + message));
      shownNotices++;
    }

    int remainingNotices = playerMessages.size() - shownNotices;
    if (remainingNotices > 0) {
      serverPlayer.sendSystemMessage(
          Component.literal("- And " + remainingNotices + " more changes."));
    }
  }

  private static void configureImportedEntity(
      EasyNPC<?> easyNPC, Vec3 position, ServerPlayer serverPlayer) {
    if (serverPlayer != null) {
      OwnerDataCapable<?> ownerData = easyNPC.getEasyNPCOwnerData();
      if (ownerData != null && !ownerData.hasNPCOwner()) {
        ownerData.setNPCOwner(serverPlayer);
      }
    }

    if (position != null) {
      NavigationDataCapable<?> navigationData = easyNPC.getEasyNPCNavigationData();
      if (navigationData != null) {
        navigationData.setHomePositionIfMissing(BlockPos.containing(position));
      }
    }
  }

  public static boolean importPreset(ServerLevel serverLevel, CompoundTag compoundTag) {
    return importPreset(serverLevel, compoundTag, null);
  }

  public static boolean importPreset(
      ServerLevel serverLevel, CompoundTag compoundTag, UUID ownerUUID) {
    PresetSanitizationResult sanitizationResult =
        SecurityManager.sanitizePresetImport(
            serverLevel,
            compoundTag,
            null,
            null,
            CommandSecurity.getServerActorContext(),
            ownerUUID);
    return importPresetData(serverLevel, sanitizationResult.sanitizedTag());
  }

  private static boolean importPresetData(ServerLevel serverLevel, CompoundTag compoundTag) {
    if (!validateImportParameters(serverLevel, compoundTag)) {
      return false;
    }

    EntityType<?> entityType = validateAndGetEntityType(compoundTag, serverLevel);
    if (entityType == null) {
      return false;
    }

    CompoundTag expandedCompoundTag =
        PresetCompactor.expand(
            compoundTag, PresetReference.getReferenceTag(entityType, serverLevel));

    UUID existingUUID =
        expandedCompoundTag.contains(Entity.UUID_TAG)
            ? expandedCompoundTag.getUUID(Entity.UUID_TAG)
            : null;
    if (existingUUID != null
        && tryUpdateExistingEntity(existingUUID, expandedCompoundTag, serverLevel)) {
      return true;
    }

    return createAndImportNewEntity(entityType, expandedCompoundTag, serverLevel);
  }

  private static boolean validateImportParameters(
      ServerLevel serverLevel, CompoundTag compoundTag) {
    if (serverLevel == null || compoundTag == null) {
      log.error("[{}] Error importing preset ", serverLevel);
      return false;
    }

    if (compoundTag.isEmpty()) {
      log.error("[{}] Empty preset data for import", serverLevel);
      return false;
    }

    return true;
  }

  private static EntityType<?> validateAndGetEntityType(
      CompoundTag compoundTag, ServerLevel serverLevel) {
    if (!compoundTag.contains(Entity.ID_TAG)) {
      log.error("[{}] Error importing preset, missing entity type", serverLevel);
      return null;
    }

    EntityType<?> entityType =
        EntityType.byString(compoundTag.getString(Entity.ID_TAG)).orElse(null);
    if (entityType == null) {
      log.error("[{}] Error importing preset, invalid entity type", serverLevel);
    }

    return entityType;
  }

  private static boolean tryUpdateExistingEntity(
      UUID uuid, CompoundTag compoundTag, ServerLevel serverLevel) {
    EasyNPC<?> existingEasyNPC =
        LivingEntityManager.getServerEasyNPCEntityByUUID(uuid, serverLevel);
    if (existingEasyNPC == null) {
      return false;
    }

    if (compoundTag.contains(Entity.ID_TAG)
        && !compoundTag.getString(Entity.ID_TAG).isEmpty()
        && compoundTag.getString(Entity.ID_TAG).equals(existingEasyNPC.getEntityTypeId())
        && existingEasyNPC.getEasyNPCPresetData() != null) {
      log.debug("[{}] Update preset data for existing entity {}!", serverLevel, existingEasyNPC);
      existingEasyNPC.getEasyNPCPresetData().importPresetData(compoundTag);
      return true;
    }

    LivingEntityManager.discardEasyNPCEntityByUUID(uuid, serverLevel);
    return false;
  }

  private static boolean createAndImportNewEntity(
      EntityType<?> entityType, CompoundTag compoundTag, ServerLevel serverLevel) {
    Entity entity = entityType.create(serverLevel);
    if (entity == null) {
      log.error("[{}] Failed to create entity of type {}", serverLevel, entityType);
      return false;
    }

    if (!(entity instanceof EasyNPC<?> easyNPCEntity)) {
      entity.discard();
      log.error("[{}] Entity type {} is not an EasyNPC", serverLevel, entityType);
      return false;
    }

    PresetDataCapable<?> presetData = easyNPCEntity.getEasyNPCPresetData();
    if (presetData == null) {
      entity.discard();
      log.error("[{}] No preset data available for {}", serverLevel, easyNPCEntity);
      return false;
    }

    try {
      easyNPCEntity.registerEasyNPCDefaultData();
      presetData.importPresetData(compoundTag);
      if (!serverLevel.addFreshEntity(easyNPCEntity.getEntity())) {
        entity.discard();
        log.error("[{}] Error spawning entity", easyNPCEntity);
        return false;
      }
      log.debug("[{}] Imported preset data {} for {}", serverLevel, compoundTag, easyNPCEntity);
      return true;
    } catch (Exception e) {
      entity.discard();
      log.error("[{}] Error importing preset data", serverLevel, e);
      return false;
    }
  }

  private static PresetData loadPresetFromSource(
      PresetType presetType, ResourceLocation presetLocation, MinecraftServer minecraftServer) {
    CompoundTag compoundTag = loadPresetCompoundTag(presetType, presetLocation, minecraftServer);
    if (compoundTag == null) {
      return null;
    }

    CompoundTag resolvedCompoundTag =
        resolveParentPresets(compoundTag, presetLocation, presetType, minecraftServer);
    if (resolvedCompoundTag == null) {
      return null;
    }

    return PresetData.fromCompoundTag(presetLocation, presetType, resolvedCompoundTag);
  }

  public static CompoundTag resolveParentPresets(
      CompoundTag compoundTag,
      ResourceLocation presetLocation,
      PresetType presetType,
      MinecraftServer minecraftServer) {
    return PresetInheritance.resolve(
        compoundTag,
        presetLocation,
        parentLocation ->
            loadPresetCompoundTag(
                SecurityManager.resolvePresetResourceType(parentLocation, presetType),
                parentLocation,
                minecraftServer));
  }

  public static CompoundTag loadPresetCompoundTag(
      PresetType presetType, ResourceLocation presetLocation, MinecraftServer minecraftServer) {
    if (presetLocation == null || minecraftServer == null) {
      return null;
    }

    SecurityDecision resourceDecision =
        SecurityManager.validatePresetResourceLocation(presetType, presetLocation);
    if (!resourceDecision.allowed()) {
      log.error(
          "Blocked invalid {} preset resource {}: {}",
          presetType,
          presetLocation,
          resourceDecision.reason());
      return null;
    }

    return switch (presetType) {
      case CUSTOM ->
          loadFromFile(
              CustomPresetDataFiles.getPresetsResourceLocationPath(presetLocation), presetLocation);
      case WORLD ->
          loadFromFile(
              WorldPresetDataFiles.getPresetsResourceLocationPath(presetLocation), presetLocation);
      case DATA, DEFAULT -> loadFromResource(presetType, presetLocation, minecraftServer);
      default -> {
        log.error("Unsupported preset type for loading: {}", presetType);
        yield null;
      }
    };
  }

  private static CompoundTag loadFromResource(
      PresetType presetType, ResourceLocation presetLocation, MinecraftServer minecraftServer) {
    try {
      var resource = minecraftServer.getResourceManager().getResource(presetLocation);
      if (resource.isEmpty()) {
        log.error("{} preset resource not found at {}", presetType, presetLocation);
        return null;
      }

      try (var inputStream = resource.get().open()) {
        PresetExportFormat format =
            PresetExportFormat.getPresetExportFormat(presetLocation.getPath());
        if (format == PresetExportFormat.SNBT) {
          return TagParser.parseTag(new String(inputStream.readAllBytes(), StandardCharsets.UTF_8));
        }

        if (format == PresetExportFormat.NBT) {
          return NbtIo.readCompressed(inputStream, NbtAccounter.unlimitedHeap());
        }

        log.error("Unknown preset format for {}", presetLocation);
        return null;
      }
    } catch (IOException exception) {
      log.error("Error reading {} preset resource {}", presetType, presetLocation, exception);
      return null;
    } catch (CommandSyntaxException exception) {
      log.error("Error parsing SNBT {} preset resource {}", presetType, presetLocation, exception);
      return null;
    }
  }

  private static CompoundTag loadFromFile(Path presetFile, ResourceLocation presetLocation) {
    if (presetFile == null) {
      log.error("Preset file path is null for: {}", presetLocation);
      return null;
    }

    return PresetFileHandler.load(presetFile.toFile());
  }

  @SuppressWarnings("unused")
  public static boolean importLocalPreset(
      ServerLevel serverLevel,
      CompoundTag compoundTag,
      ResourceLocation presetLocation,
      Vec3 position,
      UUID uuid,
      ServerPlayer serverPlayer) {
    if (serverLevel == null || presetLocation == null) {
      log.error("[{}] Error importing local preset {}", serverLevel, presetLocation);
      return false;
    }

    if (compoundTag == null || compoundTag.isEmpty()) {
      log.error(
          "[{}] Error importing local preset {}, no preset data found!",
          serverLevel,
          presetLocation);
      return false;
    }

    CompoundTag resolvedCompoundTag =
        resolveParentPresets(
            compoundTag, presetLocation, PresetType.LOCAL, serverLevel.getServer());
    if (resolvedCompoundTag == null) {
      log.error("[{}] Error resolving parent presets of local preset", serverLevel);
      return false;
    }

    PresetData presetData =
        PresetData.fromCompoundTag(presetLocation, PresetType.LOCAL, resolvedCompoundTag);
    if (presetData == null || !presetData.hasValidData()) {
      log.error("[{}] Error converting local preset to PresetData", serverLevel);
      return false;
    }

    return importPreset(serverLevel, presetData, position, uuid, serverPlayer);
  }

  public static boolean exportCustomPreset(EasyNPC<?> easyNPC, String name) {
    return exportPresetByType(easyNPC, name, CustomPresetDataFiles::getPresetFile);
  }

  public static boolean exportWorldPreset(EasyNPC<?> easyNPC, String name) {
    return exportPresetByType(easyNPC, name, WorldPresetDataFiles::getPresetFile);
  }

  private static boolean exportPresetByType(
      EasyNPC<?> easyNPC, String name, BiFunction<SkinModel, String, File> fileProvider) {
    SkinDataCapable<?> skinData = easyNPC.getEasyNPCSkinData();
    if (skinData == null) {
      log.warn("[{}] Error no skin data available!", easyNPC);
      return false;
    }

    File presetFile = fileProvider.apply(skinData.getSkinModel(), name);
    return exportPreset(easyNPC, presetFile);
  }

  public static boolean exportPreset(EasyNPC<?> easyNPC, File file) {
    if (easyNPC == null || file == null) {
      log.error("[{}] Error exporting preset {} !", easyNPC, file);
      return false;
    }

    PresetDataCapable<?> presetData = easyNPC.getEasyNPCPresetData();
    if (presetData == null) {
      log.error("[{}] Error no preset data available!", easyNPC);
      return false;
    }

    CompoundTag compoundTag = prepareExportData(easyNPC);
    if (compoundTag == null || compoundTag.isEmpty()) {
      log.error("[{}] Error exporting custom preset {}!", easyNPC, file);
      return false;
    }

    return PresetFileHandler.save(file, compoundTag);
  }

  public static CompoundTag prepareExportData(EasyNPC<?> easyNPC) {
    PresetDataCapable<?> presetData = easyNPC.getEasyNPCPresetData();
    if (presetData == null) {
      log.error("[{}] Error no preset data available!", easyNPC);
      return null;
    }

    CompoundTag compoundTag =
        SecurityManager.sanitizePresetExport(presetData.serializePresetData());
    if (compoundTag == null || compoundTag.isEmpty()) {
      return compoundTag;
    }

    PresetDataUtils.cleanupEntityData(compoundTag, PresetDataUtils.CleanupMode.FULL);
    PresetNormalizer.normalize(compoundTag);

    if (easyNPC.getEntity().level() instanceof ServerLevel serverLevel) {
      CompoundTag referenceTag =
          PresetReference.getReferenceTag(easyNPC.getEntity().getType(), serverLevel);
      return PresetCompactor.compact(compoundTag, referenceTag);
    }

    return compoundTag;
  }

  @SuppressWarnings("unused")
  public static CompoundTag prepareClientExportData(EasyNPC<?> easyNPC, PresetMetadata metadata) {
    if (easyNPC == null) {
      log.error("Cannot prepare client export data, easyNPC is null");
      return null;
    }

    CompoundTag presetData = serializeAndCopyPresetData(easyNPC);
    if (presetData == null) {
      return null;
    }

    PresetMetadata finalMetadata = extractAndEnrichMetadata(presetData, metadata, easyNPC);
    presetData.remove(PresetDataCapable.PRESET_METADATA_TAG);

    CompoundTag wrapper = new CompoundTag();
    wrapper.put(PresetDataCapable.PRESET_METADATA_TAG, finalMetadata.toCompoundTag());
    wrapper.put("data", presetData);

    log.debug(
        "[{}] Prepared client export data with metadata: {}",
        easyNPC.getEntity().getName().getString(),
        finalMetadata.category());

    return wrapper;
  }

  private static CompoundTag serializeAndCopyPresetData(EasyNPC<?> easyNPC) {
    CompoundTag presetData = prepareExportData(easyNPC);
    if (presetData == null || presetData.isEmpty()) {
      log.error("[{}] Error serializing preset data!", easyNPC);
      return null;
    }

    return presetData;
  }

  private static PresetMetadata extractAndEnrichMetadata(
      CompoundTag presetData, PresetMetadata providedMetadata, EasyNPC<?> easyNPC) {
    PresetMetadata metadata = providedMetadata;

    if (presetData.contains(PresetDataCapable.PRESET_METADATA_TAG)) {
      CompoundTag metadataTag = presetData.getCompound(PresetDataCapable.PRESET_METADATA_TAG);
      PresetMetadata extractedMetadata = PresetMetadata.fromCompoundTag(metadataTag);
      metadata = providedMetadata != null ? providedMetadata : extractedMetadata;
    } else if (metadata == null) {
      metadata = PresetMetadata.createDefault();
    }

    if (metadata.entityTypeId() == null || metadata.variantType() == null) {
      String entityTypeId = easyNPC.getEntityTypeId();
      String variantType = extractVariantType(easyNPC);
      metadata = metadata.withPreviewData(entityTypeId, variantType);
    }

    return metadata;
  }

  private static String extractVariantType(EasyNPC<?> easyNPC) {
    if (easyNPC instanceof VariantDataCapable<?> variantData) {
      Enum<?> variant = variantData.getSkinVariantType();
      if (variant != null) {
        return variant.name();
      }
    }
    return null;
  }

  @SuppressWarnings("unused")
  public static PresetData loadPreset(
      ResourceLocation presetLocation, PresetType presetType, MinecraftServer server) {
    if (presetLocation == null || server == null) {
      log.error("Cannot load preset, location or server is null");
      return null;
    }
    return loadPresetFromSource(presetType, presetLocation, server);
  }
}

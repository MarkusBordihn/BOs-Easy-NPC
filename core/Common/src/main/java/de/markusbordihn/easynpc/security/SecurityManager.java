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

package de.markusbordihn.easynpc.security;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.preset.PresetData;
import de.markusbordihn.easynpc.data.preset.PresetType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ActionEventDataCapable;
import java.util.UUID;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class SecurityManager {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private SecurityManager() {}

  public static SecurityDecision checkAccess(CommandSourceStack context, UUID uuid) {
    return AccessSecurity.checkAccess(context, uuid);
  }

  public static SecurityDecision checkAccess(ServerPlayer serverPlayer, EasyNPC<?> easyNPC) {
    return AccessSecurity.checkAccess(serverPlayer, easyNPC);
  }

  public static SecurityDecision checkAccess(ServerPlayer serverPlayer, UUID uuid) {
    return AccessSecurity.checkAccess(serverPlayer, uuid);
  }

  public static EasyNPC<?> getEasyNPCEntityByUUID(UUID uuid, ServerPlayer serverPlayer) {
    return AccessSecurity.getEasyNPCEntityByUUID(uuid, serverPlayer);
  }

  public static SecurityDecision validatePresetResourceLocation(
      PresetType presetType, ResourceLocation resourceLocation) {
    return PresetSecurity.validateResourceLocation(presetType, resourceLocation);
  }

  public static PresetSanitizationResult sanitizePresetImport(
      ServerLevel serverLevel,
      CompoundTag compoundTag,
      PresetType presetType,
      UUID targetUuid,
      ServerPlayer serverPlayer) {
    ActorSecurityContext actorSecurityContext = CommandSecurity.getActorContext(serverPlayer);
    PresetAuthority presetAuthority =
        PresetSecurity.getPresetAuthority(
            serverLevel, presetType, targetUuid, actorSecurityContext);
    PresetSanitizationResult result = PresetSanitizer.sanitize(compoundTag, presetAuthority);
    if (result.changed()) {
      log.debug("Sanitized preset import for {} with notices {}", serverPlayer, result.notices());
    }

    return result;
  }

  public static CompoundTag sanitizePresetExport(CompoundTag compoundTag) {
    return PresetSanitizer.sanitizeForExport(compoundTag);
  }

  public static PresetFeaturePreview previewPresetImport(
      ServerLevel serverLevel,
      CompoundTag compoundTag,
      PresetType presetType,
      UUID targetUuid,
      ServerPlayer serverPlayer) {
    return PresetSanitizer.preview(
        compoundTag,
        PresetSecurity.getPresetAuthority(
            serverLevel, presetType, targetUuid, CommandSecurity.getActorContext(serverPlayer)));
  }

  public static PresetFeaturePreview previewPresetImport(
      CompoundTag compoundTag, PresetAuthority presetAuthority) {
    return PresetSanitizer.preview(compoundTag, presetAuthority);
  }

  public static SecurityDecision checkFeatureAccess(
      ServerPlayer serverPlayer, EasyNPC<?> easyNPC, NpcFeature feature) {
    return FeatureSecurity.checkFeatureAccess(serverPlayer, easyNPC, feature);
  }

  public static PresetData sanitizePresetDataForSpawn(
      PresetData presetData, Level level, Player player) {
    if (presetData == null
        || !presetData.hasValidData()
        || !(level instanceof ServerLevel serverLevel)) {
      return presetData;
    }

    ServerPlayer serverPlayer = player instanceof ServerPlayer playerEntity ? playerEntity : null;
    PresetSanitizationResult result =
        sanitizePresetImport(
            serverLevel,
            presetData.data(),
            presetData.presetType(),
            presetData.getEntityUUID(),
            serverPlayer);
    return PresetData.create(
        presetData.name(),
        presetData.entityType(),
        result.sanitizedTag(),
        presetData.location(),
        presetData.presetType(),
        presetData.metadata());
  }

  public static CommandPermissionLevel applyActionAuthority(
      EasyNPC<?> easyNPC, ServerPlayer serverPlayer) {
    ActionEventDataCapable<?> actionEventData =
        easyNPC != null ? easyNPC.getEasyNPCActionEventData() : null;
    if (actionEventData == null) {
      return CommandPermissionLevel.ALL;
    }

    ActorSecurityContext actorSecurityContext = CommandSecurity.getActorContext(serverPlayer);
    PresetTrustLevel trustLevel = PresetSecurity.getTrustLevel(null, actorSecurityContext);
    CommandPermissionLevel commandPermissionLevel =
        CommandSecurity.getPresetImportCommandLevel(actorSecurityContext, trustLevel);
    actionEventData.setActionCommandPermissionLevel(commandPermissionLevel);

    return commandPermissionLevel;
  }
}

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
import de.markusbordihn.easynpc.data.preset.PresetExportFormat;
import de.markusbordihn.easynpc.data.preset.PresetType;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerDataCapable;
import de.markusbordihn.easynpc.io.DataFileHandler;
import java.util.UUID;
import net.minecraft.resources.Identifier;
import net.minecraft.server.level.ServerLevel;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class PresetSecurity {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final String NAMESPACED_DATA_PRESET_PATH_PREFIX =
      DataFileHandler.RESOURCE_NAMESPACED_PRESET_PATH + "/";
  private static final String DATA_PRESET_PATH_PREFIX = DataFileHandler.RESOURCE_PRESET_PATH + "/";
  private static final String DEFAULT_PRESET_PATH_PREFIX =
      DataFileHandler.RESOURCE_DEFAULT_PRESET_PATH + "/";

  private PresetSecurity() {}

  public static SecurityDecision validateResourceLocation(
      PresetType presetType, Identifier resourceLocation) {
    if (presetType == null || resourceLocation == null) {
      return new SecurityDecision(false, SecurityDecisionReason.INVALID_RESOURCE);
    }

    if (presetType != PresetType.DATA && presetType != PresetType.DEFAULT) {
      return new SecurityDecision(true, SecurityDecisionReason.SERVER_SOURCE);
    }

    if (PresetExportFormat.getPresetExportFormat(resourceLocation.getPath())
        == PresetExportFormat.UNKNOWN) {
      log.warn(
          "Rejected preset {}, because {} is not a known preset file format.",
          resourceLocation,
          resourceLocation.getPath());
      return new SecurityDecision(false, SecurityDecisionReason.INVALID_RESOURCE);
    }

    boolean allowedPath =
        presetType == PresetType.DATA
            ? isAllowedDataPresetPath(resourceLocation)
            : isAllowedDefaultPresetPath(resourceLocation);
    if (!allowedPath) {
      log.warn(
          "Rejected {} preset {}, because it is not below the expected {} folder.",
          presetType,
          resourceLocation,
          presetType == PresetType.DATA
              ? NAMESPACED_DATA_PRESET_PATH_PREFIX
              : DEFAULT_PRESET_PATH_PREFIX);
    }

    return new SecurityDecision(
        allowedPath,
        allowedPath
            ? SecurityDecisionReason.SERVER_SOURCE
            : SecurityDecisionReason.INVALID_RESOURCE);
  }

  private static boolean isAllowedDataPresetPath(Identifier resourceLocation) {
    if (resourceLocation.getPath().startsWith(NAMESPACED_DATA_PRESET_PATH_PREFIX)) {
      return true;
    }

    return Constants.MOD_ID.equals(resourceLocation.getNamespace())
        && resourceLocation.getPath().startsWith(DATA_PRESET_PATH_PREFIX);
  }

  private static boolean isAllowedDefaultPresetPath(Identifier resourceLocation) {
    return Constants.MOD_ID.equals(resourceLocation.getNamespace())
        && resourceLocation.getPath().startsWith(DEFAULT_PRESET_PATH_PREFIX);
  }

  public static PresetTrustLevel getTrustLevel(
      PresetType presetType, ActorSecurityContext actorSecurityContext) {
    if (actorSecurityContext == null || actorSecurityContext.player() == null) {
      return PresetTrustLevel.SERVER_TRUSTED;
    }

    if (actorSecurityContext.admin()) {
      return PresetTrustLevel.ADMIN_TRUSTED;
    }

    if (actorSecurityContext.creative()) {
      return PresetTrustLevel.CREATIVE_PLAYER;
    }

    return PresetTrustLevel.UNTRUSTED_PLAYER;
  }

  public static PresetAuthority getPresetAuthority(
      ServerLevel serverLevel,
      PresetType presetType,
      UUID targetUuid,
      ActorSecurityContext actorSecurityContext) {
    UUID ownerUuid =
        actorSecurityContext != null && actorSecurityContext.player() != null
            ? actorSecurityContext.player().getUUID()
            : null;
    return getPresetAuthority(serverLevel, presetType, targetUuid, actorSecurityContext, ownerUuid);
  }

  public static PresetAuthority getPresetAuthority(
      ServerLevel serverLevel,
      PresetType presetType,
      UUID targetUuid,
      ActorSecurityContext actorSecurityContext,
      UUID importedOwnerUuid) {
    PresetTrustLevel trustLevel = getTrustLevel(presetType, actorSecurityContext);

    return new PresetAuthority(
        getOwnerUuid(serverLevel, targetUuid, importedOwnerUuid),
        CommandSecurity.getPresetImportCommandLevel(actorSecurityContext, trustLevel),
        trustLevel,
        FeatureSecurity.getRole(actorSecurityContext));
  }

  private static UUID getOwnerUuid(
      ServerLevel serverLevel, UUID targetUuid, UUID importedOwnerUuid) {
    if (serverLevel != null && targetUuid != null) {
      EasyNPC<?> easyNPC =
          LivingEntityManager.getServerEasyNPCEntityByUUID(targetUuid, serverLevel);
      if (easyNPC != null) {
        OwnerDataCapable<?> ownerData = easyNPC.getEasyNPCOwnerData();
        if (ownerData != null && ownerData.hasNPCOwner()) {
          return ownerData.getOwnerUUID();
        }
      }
    }

    return importedOwnerUuid;
  }
}

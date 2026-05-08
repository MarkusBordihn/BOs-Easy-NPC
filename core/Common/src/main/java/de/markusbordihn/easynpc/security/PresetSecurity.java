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
import java.util.UUID;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerLevel;

public class PresetSecurity {

  private static final String DATA_PRESET_PATH_PREFIX = "preset/";
  private static final String DEFAULT_PRESET_PATH_PREFIX = "default_preset/";

  private PresetSecurity() {}

  public static SecurityDecision validateResourceLocation(
      PresetType presetType, ResourceLocation resourceLocation) {
    if (presetType == null || resourceLocation == null) {
      return new SecurityDecision(false, SecurityDecisionReason.INVALID_RESOURCE);
    }

    if (presetType != PresetType.DATA && presetType != PresetType.DEFAULT) {
      return new SecurityDecision(true, SecurityDecisionReason.SERVER_SOURCE);
    }

    if (!resourceLocation.getNamespace().equals(Constants.MOD_ID)
        || PresetExportFormat.getPresetExportFormat(resourceLocation.getPath())
            == PresetExportFormat.UNKNOWN) {
      return new SecurityDecision(false, SecurityDecisionReason.INVALID_RESOURCE);
    }

    String path = resourceLocation.getPath();
    boolean allowedPath =
        presetType == PresetType.DATA
            ? path.startsWith(DATA_PRESET_PATH_PREFIX)
            : path.startsWith(DEFAULT_PRESET_PATH_PREFIX);

    return new SecurityDecision(
        allowedPath,
        allowedPath
            ? SecurityDecisionReason.SERVER_SOURCE
            : SecurityDecisionReason.INVALID_RESOURCE);
  }

  public static PresetTrustLevel getTrustLevel(
      PresetType presetType, ActorSecurityContext actorSecurityContext) {
    if (actorSecurityContext != null && actorSecurityContext.admin()) {
      return PresetTrustLevel.ADMIN_TRUSTED;
    }

    if (actorSecurityContext != null && actorSecurityContext.creative()) {
      return PresetTrustLevel.CREATIVE_PLAYER;
    }

    if (actorSecurityContext == null
        && (presetType == PresetType.DATA || presetType == PresetType.DEFAULT)) {
      return PresetTrustLevel.SERVER_TRUSTED;
    }

    return PresetTrustLevel.UNTRUSTED_PLAYER;
  }

  public static PresetAuthority getPresetAuthority(
      ServerLevel serverLevel,
      PresetType presetType,
      UUID targetUuid,
      ActorSecurityContext actorSecurityContext) {
    PresetTrustLevel trustLevel = getTrustLevel(presetType, actorSecurityContext);

    return new PresetAuthority(
        getOwnerUuid(serverLevel, targetUuid, actorSecurityContext),
        CommandSecurity.getPresetImportCommandLevel(actorSecurityContext, trustLevel),
        trustLevel,
        FeatureSecurity.getRole(actorSecurityContext));
  }

  private static UUID getOwnerUuid(
      ServerLevel serverLevel, UUID targetUuid, ActorSecurityContext actorSecurityContext) {
    if (serverLevel != null && targetUuid != null) {
      EasyNPC<?> easyNPC = LivingEntityManager.getEasyNPCEntityByUUID(targetUuid, serverLevel);
      if (easyNPC != null) {
        OwnerDataCapable<?> ownerData = easyNPC.getEasyNPCOwnerData();
        if (ownerData != null && ownerData.hasNPCOwner()) {
          return ownerData.getOwnerUUID();
        }
      }
    }

    return actorSecurityContext != null && actorSecurityContext.player() != null
        ? actorSecurityContext.player().getUUID()
        : null;
  }
}

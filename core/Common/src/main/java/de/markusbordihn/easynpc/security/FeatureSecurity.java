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

import de.markusbordihn.easynpc.config.SecurityConfig;
import de.markusbordihn.easynpc.data.action.ActionDataType;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import net.minecraft.server.level.ServerPlayer;

public class FeatureSecurity {

  private FeatureSecurity() {}

  public static SecurityDecision checkFeatureAccess(
      ServerPlayer serverPlayer, EasyNPC<?> easyNPC, NpcFeature feature) {
    if (easyNPC == null || feature == null) {
      return new SecurityDecision(false, SecurityDecisionReason.INVALID_CONTEXT);
    }

    return checkActorFeatureAccess(serverPlayer, feature);
  }

  public static SecurityDecision checkActorFeatureAccess(
      ServerPlayer serverPlayer, NpcFeature feature) {
    return checkFeatureAccess(CommandSecurity.getActorContext(serverPlayer), feature);
  }

  public static SecurityDecision checkFeatureAccess(
      ActorSecurityContext actorSecurityContext, NpcFeature feature) {
    if (feature == null) {
      return new SecurityDecision(false, SecurityDecisionReason.INVALID_CONTEXT);
    }

    NpcSecurityRole actorRole = getRole(actorSecurityContext);
    NpcSecurityRole requiredRole = SecurityConfig.getRequiredRole(feature);
    return new SecurityDecision(
        actorRole.allows(requiredRole),
        actorRole.allows(requiredRole)
            ? SecurityDecisionReason.FEATURE_ALLOWED
            : SecurityDecisionReason.FEATURE_BLOCKED);
  }

  public static boolean isActionTypeAllowed(PresetAuthority presetAuthority, ActionDataType type) {
    return isFeatureAllowed(presetAuthority, getFeature(type));
  }

  public static boolean isFeatureAllowed(PresetAuthority presetAuthority, NpcFeature feature) {
    if (presetAuthority == null || feature == null) {
      return false;
    }

    return presetAuthority.role().allows(SecurityConfig.getRequiredRole(feature));
  }

  public static NpcFeature getFeature(ActionDataType type) {
    if (type == null) {
      return null;
    }

    return switch (type) {
      case COMMAND -> NpcFeature.COMMAND_ACTION;
      case SCOREBOARD -> NpcFeature.SCOREBOARD_ACTION;
      case INTERACT_BLOCK -> NpcFeature.INTERACT_BLOCK_ACTION;
      case OPEN_TRADING_SCREEN -> NpcFeature.OPEN_TRADING_ACTION;
      case CLOSE_DIALOG, OPEN_DEFAULT_DIALOG, OPEN_NAMED_DIALOG -> NpcFeature.DIALOG;
      default -> null;
    };
  }

  public static NpcFeature getFeature(ConfigurationType configurationType) {
    if (configurationType == null) {
      return null;
    }

    return switch (configurationType) {
      case TRADING, BASIC_TRADING, ADVANCED_TRADING, CUSTOM_TRADING, NONE_TRADING ->
          NpcFeature.TRADING;
      case BASIC_ACTION, DIALOG_ACTION, DISTANCE_ACTION -> NpcFeature.COMMAND_ACTION;
      case BASIC_OBJECTIVE, ATTACK_OBJECTIVE, FLEE_OBJECTIVE, FOLLOW_OBJECTIVE, LOOK_OBJECTIVE ->
          NpcFeature.OBJECTIVE;
      case DEFAULT_POSITION -> NpcFeature.POSITION;
      case COMBAT_ATTRIBUTE -> NpcFeature.COMBAT_ATTRIBUTE;
      case BASE_ATTRIBUTE -> NpcFeature.BASE_ATTRIBUTE;
      case WORLD_PRESET_IMPORT, WORLD_PRESET_EXPORT -> NpcFeature.WORLD_PRESET;
      case CUSTOM_PRESET_IMPORT, CUSTOM_PRESET_EXPORT -> NpcFeature.CUSTOM_PRESET;
      case LOCAL_PRESET_EXPORT -> NpcFeature.LOCAL_PRESET;
      case DEFAULT_PRESET_IMPORT -> NpcFeature.DEFAULT_PRESET_IMPORT;
      case URL_SKIN -> NpcFeature.URL_RESOURCE;
      default -> null;
    };
  }

  public static NpcSecurityRole getRole(ActorSecurityContext actorSecurityContext) {
    if (actorSecurityContext == null) {
      return NpcSecurityRole.SERVER_TRUSTED;
    }

    if (actorSecurityContext.admin()) {
      return NpcSecurityRole.ADMIN;
    }

    if (actorSecurityContext.creative()) {
      return NpcSecurityRole.CREATIVE_PLAYER;
    }

    return NpcSecurityRole.NORMAL_PLAYER;
  }
}

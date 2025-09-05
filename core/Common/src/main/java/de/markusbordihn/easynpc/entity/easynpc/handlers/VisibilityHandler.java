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

package de.markusbordihn.easynpc.entity.easynpc.handlers;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.display.DisplayAttributeType;
import de.markusbordihn.easynpc.data.display.NameVisibilityType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.DisplayAttributeDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerDataCapable;
import java.util.Objects;
import net.minecraft.world.effect.MobEffects;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.scores.Team;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class VisibilityHandler {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final double DEFAULT_NAME_VISIBILITY_RANGE = 8.0d;

  private VisibilityHandler() {}

  public static boolean handleIsInvisible(final EasyNPC<?> easyNPC, final boolean isInvisible) {

    // NPC with glow effect should always be visible
    if (easyNPC.getLivingEntity().hasEffect(MobEffects.GLOWING)) {
      return false;
    }

    // Use display attribute data to check if NPC is invisible.
    DisplayAttributeDataCapable<?> displayAttributeData = easyNPC.getEasyNPCDisplayAttributeData();
    if (displayAttributeData != null
        && displayAttributeData.hasDisplayAttribute(DisplayAttributeType.VISIBLE)
        && !displayAttributeData.getDisplayBooleanAttribute(DisplayAttributeType.VISIBLE)) {
      return true;
    }

    return isInvisible;
  }

  public static boolean handleIsInvisibleToPlayer(
      final EasyNPC<?> easyNPC, final Player player, final boolean isInvisibleToPlayers) {

    // NPC with glow effect should always be visible - this overrides ALL other settings
    if (easyNPC.getLivingEntity().hasEffect(MobEffects.GLOWING)) {
      return false;
    }

    // Use display attribute data to check if NPC is invisible to player.
    DisplayAttributeDataCapable<?> displayAttributeData = easyNPC.getEasyNPCDisplayAttributeData();
    if (displayAttributeData == null) {
      return isInvisibleToPlayers;
    }

    // Check if NPC is visible at all (master switch)
    if (displayAttributeData.hasDisplayAttribute(DisplayAttributeType.VISIBLE)
        && !displayAttributeData.getDisplayBooleanAttribute(DisplayAttributeType.VISIBLE)) {
      return true;
    }

    // Check special permissions that override other settings
    OwnerDataCapable<?> ownerData = easyNPC.getEasyNPCOwnerData();
    boolean isOwner =
        ownerData != null
            && ownerData.hasNPCOwner()
            && Objects.equals(ownerData.getOwnerUUID(), player.getUUID());
    boolean visibleToOwnerEnabled =
        displayAttributeData.hasDisplayAttribute(DisplayAttributeType.VISIBLE_TO_OWNER)
            && displayAttributeData.getDisplayBooleanAttribute(
                DisplayAttributeType.VISIBLE_TO_OWNER);

    // NPC is visible to owner
    if (isOwner && visibleToOwnerEnabled) {
      return false;
    }

    // Check if player is in same team and team visibility is enabled
    Team playerTeam = player.getTeam();
    Team npcTeam = easyNPC.getLivingEntity().getTeam();
    boolean visibleToTeamEnabled =
        displayAttributeData.hasDisplayAttribute(DisplayAttributeType.VISIBLE_TO_TEAM)
            && displayAttributeData.getDisplayBooleanAttribute(
                DisplayAttributeType.VISIBLE_TO_TEAM);
    if (npcTeam != null
        && playerTeam != null
        && npcTeam.equals(playerTeam)
        && visibleToTeamEnabled
        && npcTeam.canSeeFriendlyInvisibles()) {
      return false;
    }

    // Check game mode visibility settings
    boolean isCreativeMode = player.isCreative();
    boolean isSpectatorMode = player.isSpectator();
    boolean isStandardMode = !isCreativeMode && !isSpectatorMode;

    // Check if game mode visibility is explicitly set
    boolean gameModeVisibilitySet = false;
    boolean visibleInCurrentGameMode = false;
    if (isCreativeMode
        && displayAttributeData.hasDisplayAttribute(DisplayAttributeType.VISIBLE_IN_CREATIVE)) {
      gameModeVisibilitySet = true;
      visibleInCurrentGameMode =
          displayAttributeData.getDisplayBooleanAttribute(DisplayAttributeType.VISIBLE_IN_CREATIVE);
    } else if (isSpectatorMode
        && displayAttributeData.hasDisplayAttribute(DisplayAttributeType.VISIBLE_IN_SPECTATOR)) {
      gameModeVisibilitySet = true;
      visibleInCurrentGameMode =
          displayAttributeData.getDisplayBooleanAttribute(
              DisplayAttributeType.VISIBLE_IN_SPECTATOR);
    } else if (isStandardMode
        && displayAttributeData.hasDisplayAttribute(DisplayAttributeType.VISIBLE_IN_STANDARD)) {
      gameModeVisibilitySet = true;
      visibleInCurrentGameMode =
          displayAttributeData.getDisplayBooleanAttribute(DisplayAttributeType.VISIBLE_IN_STANDARD);
    }

    // If game mode visibility is set and NPC should be visible in this game mode
    if (gameModeVisibilitySet && visibleInCurrentGameMode) {
      return false;
    }

    // Check time-based visibility settings
    long dayTime = player.level().getDayTime() % 24000;
    boolean isDayTime = (dayTime >= 1000 && dayTime <= 13000);
    boolean isNightTime = !isDayTime;
    boolean visibleAtDaySet =
        displayAttributeData.hasDisplayAttribute(DisplayAttributeType.VISIBLE_AT_DAY);
    boolean visibleAtNightSet =
        displayAttributeData.hasDisplayAttribute(DisplayAttributeType.VISIBLE_AT_NIGHT);

    // If time visibility is explicitly set for current time
    if ((isDayTime && visibleAtDaySet) || (isNightTime && visibleAtNightSet)) {
      boolean visibleAtCurrentTime =
          isDayTime
              ? displayAttributeData.getDisplayBooleanAttribute(DisplayAttributeType.VISIBLE_AT_DAY)
              : displayAttributeData.getDisplayBooleanAttribute(
                  DisplayAttributeType.VISIBLE_AT_NIGHT);

      if (visibleAtCurrentTime) {
        return false;
      }

      // If game mode visibility isn't set, use time visibility
      if (!gameModeVisibilitySet) {
        return true;
      }
    }

    // If game mode visibility is set but not enabled for current game mode
    if (gameModeVisibilitySet && !visibleInCurrentGameMode) {
      return true;
    }

    // If we reach here and time visibility is set for the opposite time
    if ((isDayTime && !visibleAtDaySet && visibleAtNightSet)
        || (isNightTime && !visibleAtNightSet && visibleAtDaySet)) {
      return true;
    }

    return false;
  }

  public static boolean handleIsCustomNameVisible(
      final EasyNPC<?> easyNPC, final boolean isCustomNameVisible) {
    return evaluateNameVisibility(easyNPC, null, isCustomNameVisible);
  }

  public static boolean handleIsCustomNameVisibleToPlayer(
      final EasyNPC<?> easyNPC, final Player player, final boolean isCustomNameVisible) {
    return evaluateNameVisibility(easyNPC, player, isCustomNameVisible);
  }

  private static boolean evaluateNameVisibility(
      final EasyNPC<?> easyNPC, final Player player, final boolean fallbackVisibility) {

    DisplayAttributeDataCapable<?> displayAttributeData = easyNPC.getEasyNPCDisplayAttributeData();
    if (displayAttributeData == null) {
      return hasCustomNameFallback(easyNPC, fallbackVisibility);
    }

    if (!displayAttributeData.hasDisplayAttribute(DisplayAttributeType.NAME_VISIBILITY)) {
      return hasCustomNameFallback(easyNPC, fallbackVisibility);
    }

    String nameVisibilityString =
        displayAttributeData.getDisplayStringAttribute(DisplayAttributeType.NAME_VISIBILITY);

    try {
      NameVisibilityType nameVisibilityType = NameVisibilityType.valueOf(nameVisibilityString);
      return evaluateNameVisibilityType(easyNPC, player, nameVisibilityType, fallbackVisibility);
    } catch (IllegalArgumentException e) {
      log.warn("[{}] Invalid name visibility type: {}", easyNPC, nameVisibilityString);
      return hasCustomNameFallback(easyNPC, fallbackVisibility);
    }
  }

  private static boolean evaluateNameVisibilityType(
      final EasyNPC<?> easyNPC,
      final Player player,
      final NameVisibilityType nameVisibilityType,
      final boolean fallbackVisibility) {

    switch (nameVisibilityType) {
      case NEVER:
        return false;
      case ALWAYS:
        return true;
      case NEAR:
        if (!easyNPC.getEntity().hasCustomName()) {
          return false;
        }

        if (player != null) {
          double distanceSquared = easyNPC.getEntity().distanceToSqr(player);
          return distanceSquared <= DEFAULT_NAME_VISIBILITY_RANGE * DEFAULT_NAME_VISIBILITY_RANGE;
        } else {
          return fallbackVisibility;
        }

      default:
        return hasCustomNameFallback(easyNPC, fallbackVisibility);
    }
  }

  private static boolean hasCustomNameFallback(
      final EasyNPC<?> easyNPC, final boolean fallbackVisibility) {
    return easyNPC.getEntity().hasCustomName() && fallbackVisibility;
  }
}

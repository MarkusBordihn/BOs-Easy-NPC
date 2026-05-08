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
import net.minecraft.client.Minecraft;
import net.minecraft.world.effect.MobEffects;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.scores.Team;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class VisibilityHandler {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final double NEAR_NAME_VISIBILITY_RANGE = 8.0d;
  private static final double MID_NAME_VISIBILITY_RANGE = 16.0d;

  private VisibilityHandler() {}

  public static boolean handleIsInvisible(final EasyNPC<?> easyNPC, final boolean isInvisible) {

    if (easyNPC.getLivingEntity().hasEffect(MobEffects.GLOWING)) {
      return false;
    }

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

    // Glow effect overrides all visibility settings
    if (easyNPC.getLivingEntity().hasEffect(MobEffects.GLOWING)) {
      return false;
    }

    DisplayAttributeDataCapable<?> displayAttributeData = easyNPC.getEasyNPCDisplayAttributeData();
    if (displayAttributeData == null) {
      return isInvisibleToPlayers;
    }

    if (displayAttributeData.hasDisplayAttribute(DisplayAttributeType.VISIBLE)
        && !displayAttributeData.getDisplayBooleanAttribute(DisplayAttributeType.VISIBLE)) {
      return true;
    }

    OwnerDataCapable<?> ownerData = easyNPC.getEasyNPCOwnerData();
    boolean isOwner =
        ownerData != null
            && ownerData.hasNPCOwner()
            && Objects.equals(ownerData.getOwnerUUID(), player.getUUID());
    boolean visibleToOwnerEnabled =
        displayAttributeData.hasDisplayAttribute(DisplayAttributeType.VISIBLE_TO_OWNER)
            && displayAttributeData.getDisplayBooleanAttribute(
                DisplayAttributeType.VISIBLE_TO_OWNER);

    if (isOwner && visibleToOwnerEnabled) {
      return false;
    }

    Team playerTeam = player.getTeam();
    Team npcTeam = easyNPC.getLivingEntity().getTeam();
    boolean visibleToTeamEnabled =
        displayAttributeData.hasDisplayAttribute(DisplayAttributeType.VISIBLE_TO_TEAM)
            && displayAttributeData.getDisplayBooleanAttribute(
                DisplayAttributeType.VISIBLE_TO_TEAM);
    if (npcTeam != null
        && npcTeam.equals(playerTeam)
        && visibleToTeamEnabled
        && npcTeam.canSeeFriendlyInvisibles()) {
      return false;
    }

    boolean isCreativeMode = player.isCreative();
    boolean isSpectatorMode = player.isSpectator();
    boolean isStandardMode = !isCreativeMode && !isSpectatorMode;

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

    if (gameModeVisibilitySet && visibleInCurrentGameMode) {
      return false;
    }

    long dayTime = player.level().getDayTime() % 24000;
    boolean isDayTime = (dayTime >= 1000 && dayTime <= 13000);
    boolean isNightTime = !isDayTime;
    boolean visibleAtDaySet =
        displayAttributeData.hasDisplayAttribute(DisplayAttributeType.VISIBLE_AT_DAY);
    boolean visibleAtNightSet =
        displayAttributeData.hasDisplayAttribute(DisplayAttributeType.VISIBLE_AT_NIGHT);

    if ((isDayTime && visibleAtDaySet) || (isNightTime && visibleAtNightSet)) {
      boolean visibleAtCurrentTime =
          isDayTime
              ? displayAttributeData.getDisplayBooleanAttribute(DisplayAttributeType.VISIBLE_AT_DAY)
              : displayAttributeData.getDisplayBooleanAttribute(
                  DisplayAttributeType.VISIBLE_AT_NIGHT);

      if (visibleAtCurrentTime) {
        return false;
      }

      if (!gameModeVisibilitySet) {
        return true;
      }
    }

    if (gameModeVisibilitySet && !visibleInCurrentGameMode) {
      return true;
    }

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
    if (easyNPC.getEntity().level().isClientSide() && Minecraft.getInstance().options.hideGui) {
      return false;
    }

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

    return switch (nameVisibilityType) {
      case NEVER -> false;
      case ALWAYS -> true;
      case NEAR ->
          evaluateDistanceBasedVisibility(
              easyNPC, player, NEAR_NAME_VISIBILITY_RANGE, fallbackVisibility);
      case MID ->
          evaluateDistanceBasedVisibility(
              easyNPC, player, MID_NAME_VISIBILITY_RANGE, fallbackVisibility);
      case MOUSE_OVER -> evaluateMouseOverVisibility(easyNPC);
      default -> hasCustomNameFallback(easyNPC, fallbackVisibility);
    };
  }

  private static boolean evaluateDistanceBasedVisibility(
      final EasyNPC<?> easyNPC,
      final Player player,
      final double range,
      final boolean fallbackVisibility) {
    if (!easyNPC.getEntity().hasCustomName()) {
      return false;
    }

    if (player == null) {
      return fallbackVisibility;
    }

    // Check distance first
    double distanceSquared = easyNPC.getEntity().distanceToSqr(player);
    double rangeSquared = range * range;
    if (distanceSquared > rangeSquared) {
      return false;
    }

    Team npcTeam = easyNPC.getLivingEntity().getTeam();
    if (npcTeam != null) {
      Team.Visibility teamNameTagVisibility = npcTeam.getNameTagVisibility();
      return switch (teamNameTagVisibility) {
        case NEVER -> false;
        case HIDE_FOR_OTHER_TEAMS -> {
          Team playerTeam = player.getTeam();
          yield playerTeam != null
              && npcTeam.isAlliedTo(playerTeam)
              && (npcTeam.canSeeFriendlyInvisibles() || !easyNPC.getEntity().isInvisibleTo(player));
        }
        case HIDE_FOR_OWN_TEAM -> {
          Team playerOwnTeam = player.getTeam();
          yield playerOwnTeam == null
              || !npcTeam.isAlliedTo(playerOwnTeam) && !easyNPC.getEntity().isInvisibleTo(player);
        }
        default -> !easyNPC.getEntity().isInvisibleTo(player);
      };
    }

    return !easyNPC.getEntity().isInvisibleTo(player);
  }

  private static boolean evaluateMouseOverVisibility(final EasyNPC<?> easyNPC) {
    if (!easyNPC.getEntity().hasCustomName()) {
      return false;
    }

    if (!easyNPC.getEntity().level().isClientSide()) {
      return true;
    }

    Minecraft minecraft = Minecraft.getInstance();
    if (minecraft.cameraEntity == null || minecraft.crosshairPickEntity == null) {
      return false;
    }

    return minecraft.crosshairPickEntity == easyNPC.getEntity() && !easyNPC.getEntity().isVehicle();
  }

  private static boolean hasCustomNameFallback(
      final EasyNPC<?> easyNPC, final boolean fallbackVisibility) {
    return easyNPC.getEntity().hasCustomName() && fallbackVisibility;
  }
}

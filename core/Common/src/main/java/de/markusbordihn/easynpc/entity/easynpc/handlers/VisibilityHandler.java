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
import de.markusbordihn.easynpc.data.display.DisplayAttributeDataSet;
import de.markusbordihn.easynpc.data.display.DisplayAttributeType;
import de.markusbordihn.easynpc.data.display.NameVisibilityType;
import de.markusbordihn.easynpc.data.highlight.NPCHighlightManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.DisplayAttributeDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerDataCapable;
import de.markusbordihn.easynpc.utils.ItemUtils;
import java.util.Objects;
import net.minecraft.client.Minecraft;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import net.minecraft.world.scores.Team;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class VisibilityHandler {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final double NEAR_NAME_VISIBILITY_RANGE = 8.0d;
  private static final double MID_NAME_VISIBILITY_RANGE = 16.0d;
  private static final long DAY_LENGTH = 24000L;
  private static final long DAY_TIME_END = 12000L;

  private VisibilityHandler() {}

  public static boolean handleIsInvisible(final EasyNPC<?> easyNPC, final boolean isInvisible) {

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

    if (isHighlightedForPlayer(easyNPC, player)) {
      return false;
    }

    DisplayAttributeDataCapable<?> displayAttributeData = easyNPC.getEasyNPCDisplayAttributeData();
    if (displayAttributeData == null) {
      return isInvisibleToPlayers;
    }

    return !isVisible(
        easyNPC,
        displayAttributeData.getDisplayAttributeData(),
        player,
        player.level().getDayTime());
  }

  private static boolean isHighlightedForPlayer(final EasyNPC<?> easyNPC, final Player player) {
    if (ItemUtils.isPlayerHoldingEasyNPCWand(player)) {
      return true;
    }

    return easyNPC.getEntity().level().isClientSide()
        && NPCHighlightManager.isHighlighted(easyNPC.getEntity().getUUID());
  }

  public static boolean isVisibleToPlayerAtDayTime(
      final EasyNPC<?> easyNPC, final Player player, final long dayTime) {
    DisplayAttributeDataCapable<?> displayAttributeData = easyNPC.getEasyNPCDisplayAttributeData();
    return displayAttributeData == null
        || isVisible(easyNPC, displayAttributeData.getDisplayAttributeData(), player, dayTime);
  }

  private static boolean isVisible(
      final EasyNPC<?> easyNPC,
      final DisplayAttributeDataSet displayAttributes,
      final Player player,
      final long dayTime) {

    return displayAttributes.booleanValue(DisplayAttributeType.VISIBLE)
        && isVisibleAtDayTime(displayAttributes, player.level(), dayTime)
        && isVisibleInGameMode(displayAttributes, player)
        && isVisibleToOwner(easyNPC, displayAttributes, player)
        && isVisibleToTeam(easyNPC, displayAttributes, player);
  }

  private static boolean isVisibleAtDayTime(
      final DisplayAttributeDataSet displayAttributes, final Level level, final long dayTime) {
    if (level.dimensionType().hasFixedTime()) {
      return true;
    }

    boolean isDayTime = dayTime % DAY_LENGTH < DAY_TIME_END;
    return displayAttributes.booleanValue(
        isDayTime ? DisplayAttributeType.VISIBLE_AT_DAY : DisplayAttributeType.VISIBLE_AT_NIGHT);
  }

  private static boolean isVisibleInGameMode(
      final DisplayAttributeDataSet displayAttributes, final Player player) {
    if (player.isSpectator()) {
      return displayAttributes.booleanValue(DisplayAttributeType.VISIBLE_IN_SPECTATOR);
    }

    if (player.isCreative()) {
      return displayAttributes.booleanValue(DisplayAttributeType.VISIBLE_IN_CREATIVE);
    }

    return displayAttributes.booleanValue(DisplayAttributeType.VISIBLE_IN_STANDARD);
  }

  private static boolean isVisibleToOwner(
      final EasyNPC<?> easyNPC,
      final DisplayAttributeDataSet displayAttributes,
      final Player player) {
    if (displayAttributes.booleanValue(DisplayAttributeType.VISIBLE_TO_OWNER)) {
      return true;
    }

    OwnerDataCapable<?> ownerData = easyNPC.getEasyNPCOwnerData();
    return ownerData == null || !Objects.equals(ownerData.getOwnerUUID(), player.getUUID());
  }

  private static boolean isVisibleToTeam(
      final EasyNPC<?> easyNPC,
      final DisplayAttributeDataSet displayAttributes,
      final Player player) {
    if (displayAttributes.booleanValue(DisplayAttributeType.VISIBLE_TO_TEAM)) {
      return true;
    }

    Team npcTeam = easyNPC.getLivingEntity().getTeam();
    return npcTeam == null || !npcTeam.equals(player.getTeam());
  }

  public static boolean handleIsCustomNameVisible(
      final EasyNPC<?> easyNPC, final boolean isCustomNameVisible) {
    return evaluateNameVisibility(easyNPC, null, isCustomNameVisible, -1.0);
  }

  public static boolean handleIsCustomNameVisibleToPlayer(
      final EasyNPC<?> easyNPC,
      final Player player,
      final boolean isCustomNameVisible,
      final double distanceSquared) {
    if (easyNPC.getEntity().level().isClientSide() && Minecraft.getInstance().options.hideGui) {
      return false;
    }

    return evaluateNameVisibility(easyNPC, player, isCustomNameVisible, distanceSquared);
  }

  private static boolean evaluateNameVisibility(
      final EasyNPC<?> easyNPC,
      final Player player,
      final boolean fallbackVisibility,
      final double distanceSquared) {

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
      return evaluateNameVisibilityType(
          easyNPC, player, nameVisibilityType, fallbackVisibility, distanceSquared);
    } catch (IllegalArgumentException e) {
      log.warn("[{}] Invalid name visibility type: {}", easyNPC, nameVisibilityString);
      return hasCustomNameFallback(easyNPC, fallbackVisibility);
    }
  }

  private static boolean evaluateNameVisibilityType(
      final EasyNPC<?> easyNPC,
      final Player player,
      final NameVisibilityType nameVisibilityType,
      final boolean fallbackVisibility,
      final double distanceSquared) {

    return switch (nameVisibilityType) {
      case NEVER -> false;
      case ALWAYS -> true;
      case NEAR ->
          evaluateDistanceBasedVisibility(
              easyNPC, player, NEAR_NAME_VISIBILITY_RANGE, fallbackVisibility, distanceSquared);
      case MID ->
          evaluateDistanceBasedVisibility(
              easyNPC, player, MID_NAME_VISIBILITY_RANGE, fallbackVisibility, distanceSquared);
      case MOUSE_OVER -> evaluateMouseOverVisibility(easyNPC);
      default -> hasCustomNameFallback(easyNPC, fallbackVisibility);
    };
  }

  private static boolean evaluateDistanceBasedVisibility(
      final EasyNPC<?> easyNPC,
      final Player player,
      final double range,
      final boolean fallbackVisibility,
      final double providedDistanceSquared) {
    if (!easyNPC.getEntity().hasCustomName()) {
      return false;
    }

    if (player == null) {
      return fallbackVisibility;
    }

    // Use provided distance if available (from Minecraft's shouldShowName), otherwise calculate
    double distanceSquared =
        providedDistanceSquared >= 0.0
            ? providedDistanceSquared
            : easyNPC.getEntity().distanceToSqr(player);
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
    if (minecraft.getCameraEntity() == null || minecraft.crosshairPickEntity == null) {
      return false;
    }

    return minecraft.crosshairPickEntity == easyNPC.getEntity() && !easyNPC.getEntity().isVehicle();
  }

  private static boolean hasCustomNameFallback(
      final EasyNPC<?> easyNPC, final boolean fallbackVisibility) {
    return easyNPC.getEntity().hasCustomName() && fallbackVisibility;
  }
}

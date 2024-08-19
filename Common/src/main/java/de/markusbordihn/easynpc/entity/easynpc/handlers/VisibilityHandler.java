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
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.DisplayAttributeData;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerData;
import java.util.Objects;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.scores.Team;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class VisibilityHandler {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private VisibilityHandler() {}

  public static boolean handleIsInvisible(EasyNPC<?> easyNPC, boolean isInvisible) {
    // Use display attribute data to check if NPC is invisible.
    DisplayAttributeData<?> displayAttributeData = easyNPC.getEasyNPCDisplayAttributeData();
    if (displayAttributeData != null
        && displayAttributeData.hasDisplayAttribute(DisplayAttributeType.VISIBLE)
        && !displayAttributeData.getDisplayBooleanAttribute(DisplayAttributeType.VISIBLE)) {
      return true;
    }
    return isInvisible;
  }

  public static boolean handleIsInvisibleToPlayer(
      EasyNPC<?> easyNPC, Player player, boolean isInvisibleToPlayers) {

    // Use display attribute data to check if NPC is invisible to player.
    DisplayAttributeData<?> displayAttributeData = easyNPC.getEasyNPCDisplayAttributeData();
    if (displayAttributeData == null) {
      return isInvisibleToPlayers;
    }

    // Check if NPC is visible at all
    if (displayAttributeData.hasDisplayAttribute(DisplayAttributeType.VISIBLE)
        && !displayAttributeData.getDisplayBooleanAttribute(DisplayAttributeType.VISIBLE)) {
      return true;
    }

    // Check specific visibility attributes
    boolean isVisible = true;
    LivingEntity livingEntity = easyNPC.getLivingEntity();
    long dayTime = player.level().getDayTime();

    // NPC is visible at day
    if (displayAttributeData.hasDisplayAttribute(DisplayAttributeType.VISIBLE_AT_DAY)
        && !displayAttributeData.getDisplayBooleanAttribute(DisplayAttributeType.VISIBLE_AT_DAY)
        && (dayTime >= 1000 && dayTime <= 13000)) {
      isVisible = false;
    }

    // NPC is visible at night
    if (displayAttributeData.hasDisplayAttribute(DisplayAttributeType.VISIBLE_AT_NIGHT)
        && !displayAttributeData.getDisplayBooleanAttribute(DisplayAttributeType.VISIBLE_AT_NIGHT)
        && (dayTime < 1000 || dayTime > 13000)) {
      isVisible = false;
    }

    // Visible in creative mode
    if (displayAttributeData.hasDisplayAttribute(DisplayAttributeType.VISIBLE_IN_CREATIVE)
        && !displayAttributeData.getDisplayBooleanAttribute(
            DisplayAttributeType.VISIBLE_IN_CREATIVE)
        && player.isCreative()) {
      isVisible = false;
    }

    // Visible in spectator mode
    if (displayAttributeData.hasDisplayAttribute(DisplayAttributeType.VISIBLE_IN_SPECTATOR)
        && !displayAttributeData.getDisplayBooleanAttribute(
            DisplayAttributeType.VISIBLE_IN_SPECTATOR)
        && player.isSpectator()) {
      isVisible = false;
    }

    // Visible in standard mode
    if (displayAttributeData.hasDisplayAttribute(DisplayAttributeType.VISIBLE_IN_STANDARD)
        && !displayAttributeData.getDisplayBooleanAttribute(
            DisplayAttributeType.VISIBLE_IN_STANDARD)
        && !player.isCreative()
        && !player.isSpectator()) {
      isVisible = false;
    }

    // Check if NPC is visible to owner (overrides other visibility settings)
    OwnerData<?> ownerData = easyNPC.getEasyNPCOwnerData();
    if (displayAttributeData.hasDisplayAttribute(DisplayAttributeType.VISIBLE_TO_OWNER)
        && displayAttributeData.getDisplayBooleanAttribute(DisplayAttributeType.VISIBLE_TO_OWNER)
        && ownerData != null
        && ownerData.hasOwner()
        && Objects.equals(ownerData.getOwnerUUID(), player.getUUID())) {
      isVisible = true;
    }

    // Check if NPC is visible to team (overrides other visibility settings)
    Team playerTeam = player.getTeam();
    Team livingEntityTeam = livingEntity.getTeam();
    if (displayAttributeData.hasDisplayAttribute(DisplayAttributeType.VISIBLE_TO_TEAM)
        && displayAttributeData.getDisplayBooleanAttribute(DisplayAttributeType.VISIBLE_TO_TEAM)
        && livingEntityTeam != null
        && livingEntityTeam.equals(playerTeam)) {
      isVisible = livingEntityTeam.canSeeFriendlyInvisibles();
    }

    return !isVisible;
  }
}

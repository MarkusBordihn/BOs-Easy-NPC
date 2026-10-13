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

package de.markusbordihn.easynpc.entity.easynpc.handlers;

import de.markusbordihn.easynpc.data.display.DisplayAttributeType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.DisplayAttributeDataCapable;
import java.util.Collection;
import java.util.List;
import net.minecraft.server.level.ServerBossEvent;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.util.Mth;
import net.minecraft.world.BossEvent;
import net.minecraft.world.entity.LivingEntity;

public class BossBarHandler {

  private static final int UPDATE_INTERVAL_TICKS = 10;

  private ServerBossEvent bossEvent;

  public static BossEvent.BossBarColor getBossBarColor(
      DisplayAttributeDataCapable<?> displayAttributeData) {
    try {
      return BossEvent.BossBarColor.valueOf(
          displayAttributeData.getDisplayStringAttribute(DisplayAttributeType.BOSS_BAR_COLOR));
    } catch (IllegalArgumentException e) {
      return BossEvent.BossBarColor.PURPLE;
    }
  }

  public static BossEvent.BossBarOverlay getBossBarStyle(
      DisplayAttributeDataCapable<?> displayAttributeData) {
    try {
      return BossEvent.BossBarOverlay.valueOf(
          displayAttributeData.getDisplayStringAttribute(DisplayAttributeType.BOSS_BAR_STYLE));
    } catch (IllegalArgumentException e) {
      return BossEvent.BossBarOverlay.PROGRESS;
    }
  }

  public static int getBossBarRange(DisplayAttributeDataCapable<?> displayAttributeData) {
    return Mth.clamp(
        displayAttributeData.getDisplayIntAttribute(DisplayAttributeType.BOSS_BAR_RANGE),
        DisplayAttributeType.MIN_BOSS_BAR_RANGE,
        DisplayAttributeType.MAX_BOSS_BAR_RANGE);
  }

  public void tick(EasyNPC<?> easyNPC) {
    LivingEntity livingEntity = easyNPC.getLivingEntity();
    if (!(livingEntity.level() instanceof ServerLevel serverLevel)
        || livingEntity.tickCount % UPDATE_INTERVAL_TICKS != 0) {
      return;
    }

    DisplayAttributeDataCapable<?> displayAttributeData = easyNPC.getEasyNPCDisplayAttributeData();
    if (displayAttributeData == null
        || !displayAttributeData.getDisplayBooleanAttribute(DisplayAttributeType.SHOW_BOSS_BAR)
        || !livingEntity.isAlive()) {
      this.removeAllPlayers();
      return;
    }

    if (this.bossEvent == null) {
      this.bossEvent =
          new ServerBossEvent(
              Mth.createInsecureUUID(livingEntity.getRandom()),
              livingEntity.getDisplayName(),
              getBossBarColor(displayAttributeData),
              getBossBarStyle(displayAttributeData));
    }
    this.bossEvent.setName(livingEntity.getDisplayName());
    this.bossEvent.setColor(getBossBarColor(displayAttributeData));
    this.bossEvent.setOverlay(getBossBarStyle(displayAttributeData));
    this.bossEvent.setProgress(livingEntity.getHealth() / livingEntity.getMaxHealth());

    int range = getBossBarRange(displayAttributeData);
    double rangeSquared = (double) range * range;
    for (ServerPlayer serverPlayer : List.copyOf(this.bossEvent.getPlayers())) {
      if (!this.isInRange(easyNPC, serverLevel, serverPlayer, rangeSquared)) {
        this.bossEvent.removePlayer(serverPlayer);
      }
    }
    for (ServerPlayer serverPlayer : serverLevel.players()) {
      if (this.isInRange(easyNPC, serverLevel, serverPlayer, rangeSquared)) {
        this.bossEvent.addPlayer(serverPlayer);
      }
    }
  }

  public Collection<ServerPlayer> getPlayers() {
    return this.bossEvent == null ? List.of() : this.bossEvent.getPlayers();
  }

  public void removeAllPlayers() {
    if (this.bossEvent != null) {
      this.bossEvent.removeAllPlayers();
      this.bossEvent = null;
    }
  }

  private boolean isInRange(
      EasyNPC<?> easyNPC, ServerLevel serverLevel, ServerPlayer serverPlayer, double rangeSquared) {
    return !serverPlayer.isRemoved()
        && serverPlayer.level() == serverLevel
        && serverPlayer.distanceToSqr(easyNPC.getEntity()) <= rangeSquared
        && VisibilityHandler.isVisibleToPlayerAtDayTime(
            easyNPC, serverPlayer, serverLevel.getDefaultClockTime());
  }
}

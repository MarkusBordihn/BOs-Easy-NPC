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
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import net.minecraft.server.level.ServerBossEvent;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.util.Mth;
import net.minecraft.world.BossEvent;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;

public class BossBarHandler {

  private static final int UPDATE_INTERVAL_TICKS = 10;

  private final Set<ServerPlayer> seenByPlayers = new HashSet<>();
  private final Map<ServerPlayer, Integer> visibleSinceTicks = new HashMap<>();
  private final Set<ServerPlayer> autoHiddenPlayers = new HashSet<>();
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

  public static int getBossBarAutoHideSeconds(
      DisplayAttributeDataCapable<?> displayAttributeData) {
    return Mth.clamp(
        displayAttributeData.getDisplayIntAttribute(DisplayAttributeType.BOSS_BAR_AUTO_HIDE),
        DisplayAttributeType.MIN_BOSS_BAR_AUTO_HIDE_SECONDS,
        DisplayAttributeType.MAX_BOSS_BAR_AUTO_HIDE_SECONDS);
  }

  private static boolean isFighting(LivingEntity livingEntity) {
    if (livingEntity instanceof Mob mob && mob.getTarget() != null) {
      return true;
    }

    return livingEntity.getLastHurtByMob() != null
        && livingEntity.tickCount - livingEntity.getLastHurtByMobTimestamp()
            <= UPDATE_INTERVAL_TICKS;
  }

  public void startSeenByPlayer(ServerPlayer serverPlayer) {
    this.seenByPlayers.add(serverPlayer);
  }

  public void stopSeenByPlayer(ServerPlayer serverPlayer) {
    this.seenByPlayers.remove(serverPlayer);
    this.removePlayer(serverPlayer);
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
    Set<ServerPlayer> playersInRange = new HashSet<>();
    for (ServerPlayer serverPlayer : this.seenByPlayers) {
      if (this.isInRange(easyNPC, serverLevel, serverPlayer, rangeSquared)) {
        playersInRange.add(serverPlayer);
      }
    }
    for (ServerPlayer serverPlayer : List.copyOf(this.bossEvent.getPlayers())) {
      if (!playersInRange.contains(serverPlayer)) {
        this.bossEvent.removePlayer(serverPlayer);
      }
    }
    this.visibleSinceTicks.keySet().retainAll(playersInRange);
    this.autoHiddenPlayers.retainAll(playersInRange);

    if (isFighting(livingEntity)) {
      this.visibleSinceTicks.clear();
      this.autoHiddenPlayers.clear();
    }

    int autoHideTicks = getBossBarAutoHideSeconds(displayAttributeData) * 20;
    int tickCount = livingEntity.tickCount;
    for (ServerPlayer serverPlayer : playersInRange) {
      if (this.autoHiddenPlayers.contains(serverPlayer)) {
        continue;
      }

      int visibleSinceTick =
          this.visibleSinceTicks.computeIfAbsent(serverPlayer, player -> tickCount);
      if (autoHideTicks > 0 && tickCount - visibleSinceTick >= autoHideTicks) {
        this.bossEvent.removePlayer(serverPlayer);
        this.autoHiddenPlayers.add(serverPlayer);
      } else {
        this.bossEvent.addPlayer(serverPlayer);
      }
    }
  }

  public Collection<ServerPlayer> getPlayers() {
    return this.bossEvent == null ? List.of() : this.bossEvent.getPlayers();
  }

  public void removePlayer(ServerPlayer serverPlayer) {
    this.visibleSinceTicks.remove(serverPlayer);
    this.autoHiddenPlayers.remove(serverPlayer);
    if (this.bossEvent != null) {
      this.bossEvent.removePlayer(serverPlayer);
    }
  }

  public void removeAllPlayers() {
    this.visibleSinceTicks.clear();
    this.autoHiddenPlayers.clear();
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

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

package de.markusbordihn.easynpc.handler;

import de.markusbordihn.easynpc.network.components.TextComponent;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.LivingEntity;

public class FactionDisputeNotifier {

  public static final int MESSAGE_VARIANTS = 4;

  private static final int MESSAGE_COOLDOWN_TICKS = 200;
  private static final int MAX_TRACKED_DISPUTES = 64;
  private static final Map<UUID, Long> lastNotificationPerVictim = new HashMap<>();

  private FactionDisputeNotifier() {}

  public static void notifyInternalDispute(
      LivingEntity victim, LivingEntity attacker, String factionName) {
    if (!(victim instanceof ServerPlayer) && !(attacker instanceof ServerPlayer)) {
      return;
    }

    long gameTime = victim.level().getGameTime();
    if (!isNotificationDue(victim.getUUID(), gameTime)) {
      return;
    }

    Component message =
        TextComponent.getTranslatedText(
            "faction.internal_dispute." + (gameTime % MESSAGE_VARIANTS), factionName);
    if (victim instanceof ServerPlayer victimPlayer) {
      victimPlayer.sendSystemMessage(message);
    }
    if (attacker instanceof ServerPlayer attackerPlayer) {
      attackerPlayer.sendSystemMessage(message);
    }
  }

  public static void clearNotifications() {
    synchronized (lastNotificationPerVictim) {
      lastNotificationPerVictim.clear();
    }
  }

  private static boolean isNotificationDue(UUID victimUUID, long gameTime) {
    synchronized (lastNotificationPerVictim) {
      Long lastNotification = lastNotificationPerVictim.get(victimUUID);
      if (lastNotification != null && gameTime - lastNotification < MESSAGE_COOLDOWN_TICKS) {
        return false;
      }

      if (lastNotificationPerVictim.size() >= MAX_TRACKED_DISPUTES) {
        lastNotificationPerVictim
            .entrySet()
            .removeIf(entry -> gameTime - entry.getValue() >= MESSAGE_COOLDOWN_TICKS);
      }

      lastNotificationPerVictim.put(victimUUID, gameTime);
      return true;
    }
  }
}

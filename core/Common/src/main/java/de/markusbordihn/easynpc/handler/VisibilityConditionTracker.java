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

import de.markusbordihn.easynpc.condition.ConditionManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.DisplayAttributeDataCapable;
import de.markusbordihn.easynpc.network.NetworkHandlerManager;
import de.markusbordihn.easynpc.network.message.client.UpdateEasyNPCVisibilityMessage;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerPlayer;

public class VisibilityConditionTracker {

  private static final int CHECK_INTERVAL_TICKS = 20;
  private static final Map<UUID, TrackedNPC> trackedNPCs = new ConcurrentHashMap<>();

  private VisibilityConditionTracker() {}

  public static void startSeenByPlayer(EasyNPC<?> easyNPC, ServerPlayer serverPlayer) {
    if (easyNPC == null || serverPlayer == null || easyNPC.isClientSideInstance()) {
      return;
    }

    TrackedNPC trackedNPC = trackedNPCs.get(easyNPC.getEntityUUID());
    if (trackedNPC == null || trackedNPC.easyNPC() != easyNPC) {
      trackedNPC = new TrackedNPC(easyNPC, new ConcurrentHashMap<>());
      trackedNPCs.put(easyNPC.getEntityUUID(), trackedNPC);
    }
    Viewer viewer = new Viewer(serverPlayer, null);
    trackedNPC.viewers().put(serverPlayer.getUUID(), viewer);
    if (hasVisibilityConditions(easyNPC)) {
      update(trackedNPC, viewer);
    }
  }

  public static void stopSeenByPlayer(EasyNPC<?> easyNPC, ServerPlayer serverPlayer) {
    if (easyNPC == null || serverPlayer == null || easyNPC.isClientSideInstance()) {
      return;
    }

    TrackedNPC trackedNPC = trackedNPCs.get(easyNPC.getEntityUUID());
    if (trackedNPC == null) {
      return;
    }

    trackedNPC.viewers().remove(serverPlayer.getUUID());
    if (trackedNPC.viewers().isEmpty()) {
      trackedNPCs.remove(easyNPC.getEntityUUID(), trackedNPC);
    }
  }

  public static void removeNPC(EasyNPC<?> easyNPC) {
    if (easyNPC == null || easyNPC.isClientSideInstance()) {
      return;
    }

    trackedNPCs.computeIfPresent(
        easyNPC.getEntityUUID(),
        (uuid, trackedNPC) -> trackedNPC.easyNPC() == easyNPC ? null : trackedNPC);
  }

  public static void refresh(EasyNPC<?> easyNPC) {
    if (easyNPC == null || easyNPC.isClientSideInstance()) {
      return;
    }

    TrackedNPC trackedNPC = trackedNPCs.get(easyNPC.getEntityUUID());
    if (trackedNPC == null || trackedNPC.easyNPC() != easyNPC) {
      return;
    }

    boolean hasVisibilityConditions = hasVisibilityConditions(easyNPC);
    for (Viewer viewer : trackedNPC.viewers().values()) {
      Viewer unknownViewer = new Viewer(viewer.serverPlayer(), null);
      trackedNPC.viewers().put(viewer.serverPlayer().getUUID(), unknownViewer);
      if (hasVisibilityConditions) {
        update(trackedNPC, unknownViewer);
      }
    }
  }

  public static void handleServerTick(MinecraftServer minecraftServer) {
    if (minecraftServer == null
        || trackedNPCs.isEmpty()
        || minecraftServer.getTickCount() % CHECK_INTERVAL_TICKS != 0) {
      return;
    }

    trackedNPCs.values().removeIf(trackedNPC -> trackedNPC.easyNPC().getEntity().isRemoved());
    for (TrackedNPC trackedNPC : trackedNPCs.values()) {
      if (!hasVisibilityConditions(trackedNPC.easyNPC())) {
        continue;
      }

      for (Viewer viewer : trackedNPC.viewers().values()) {
        update(trackedNPC, viewer);
      }
    }
  }

  public static boolean isVisible(EasyNPC<?> easyNPC, ServerPlayer serverPlayer) {
    TrackedNPC trackedNPC = trackedNPCs.get(easyNPC.getEntityUUID());
    Viewer viewer =
        trackedNPC != null && trackedNPC.easyNPC() == easyNPC
            ? trackedNPC.viewers().get(serverPlayer.getUUID())
            : null;
    if (viewer != null && viewer.visible() != null) {
      return viewer.visible();
    }

    return evaluate(easyNPC, serverPlayer);
  }

  public static void reset() {
    trackedNPCs.clear();
  }

  private static boolean hasVisibilityConditions(EasyNPC<?> easyNPC) {
    DisplayAttributeDataCapable<?> displayAttributeData = easyNPC.getEasyNPCDisplayAttributeData();
    return displayAttributeData != null && displayAttributeData.hasVisibilityConditions();
  }

  private static boolean evaluate(EasyNPC<?> easyNPC, ServerPlayer serverPlayer) {
    DisplayAttributeDataCapable<?> displayAttributeData = easyNPC.getEasyNPCDisplayAttributeData();
    return displayAttributeData == null
        || ConditionManager.evaluateAll(
            displayAttributeData.getVisibilityConditionSet().conditionSet(),
            serverPlayer,
            null,
            easyNPC.getLivingEntity());
  }

  private static void update(TrackedNPC trackedNPC, Viewer viewer) {
    boolean visible = evaluate(trackedNPC.easyNPC(), viewer.serverPlayer());
    if (viewer.visible() != null && viewer.visible() == visible) {
      return;
    }

    trackedNPC
        .viewers()
        .put(viewer.serverPlayer().getUUID(), new Viewer(viewer.serverPlayer(), visible));
    NetworkHandlerManager.sendMessageToPlayer(
        new UpdateEasyNPCVisibilityMessage(trackedNPC.easyNPC().getEntityUUID(), visible),
        viewer.serverPlayer());
  }

  private record TrackedNPC(EasyNPC<?> easyNPC, Map<UUID, Viewer> viewers) {}

  private record Viewer(ServerPlayer serverPlayer, Boolean visible) {}
}

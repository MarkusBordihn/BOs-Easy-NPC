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

package de.markusbordihn.easynpc.client.renderer.entity;

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.client.renderer.entity.SpeechBubblePlacement.OverlapOffsets;
import de.markusbordihn.easynpc.client.renderer.entity.SpeechBubblePlacement.ScreenRect;
import de.markusbordihn.easynpc.config.ClientSpeechBubbleConfig;
import de.markusbordihn.easynpc.data.action.SpeechBubbleManager;
import de.markusbordihn.easynpc.data.action.SpeechBubbleManager.SpeechBubbleEntry;
import de.markusbordihn.easynpc.data.display.DisplayAttributeType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.handler.AttributeHandler;
import de.markusbordihn.easynpc.utils.ItemUtils;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import net.minecraft.client.Camera;
import net.minecraft.client.Minecraft;
import net.minecraft.client.player.LocalPlayer;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.phys.Vec3;
import org.joml.Matrix4f;

public class SpeechBubbleFrameRenderer {

  private static final double NPC_WAND_RENDER_RANGE = 32.0D;
  private static final float OFFSET_SMOOTHING = 0.25F;
  private static final Map<UUID, float[]> smoothedOffsets = new HashMap<>();

  private SpeechBubbleFrameRenderer() {}

  public static void clear() {
    smoothedOffsets.clear();
  }

  public static void renderFrame(
      Minecraft minecraft,
      PoseStack poseStack,
      MultiBufferSource.BufferSource bufferSource,
      Camera camera,
      Matrix4f projectionMatrix,
      float partialTick) {
    if (SpeechBubbleManager.isEmpty() || minecraft.level == null || minecraft.player == null) {
      smoothedOffsets.clear();
      return;
    }

    smoothedOffsets.keySet().removeIf(uuid -> SpeechBubbleManager.get(uuid) == null);

    float projectionScaleX = projectionMatrix.m00();
    float projectionScaleY = projectionMatrix.m11();
    List<SpeechBubbleInstance> speechBubbleInstances = new ArrayList<>();
    List<ScreenRect> screenRects = new ArrayList<>();

    for (Entity entity : minecraft.level.entitiesForRendering()) {
      SpeechBubbleEntry speechBubbleEntry = SpeechBubbleManager.get(entity.getUUID());
      if (speechBubbleEntry == null || !isSpeechBubbleVisible(entity, minecraft.player)) {
        continue;
      }

      SpeechBubbleInstance speechBubbleInstance =
          SpeechBubbleRenderer.measure(
              entity,
              speechBubbleEntry,
              minecraft,
              partialTick,
              minecraft.getEntityRenderDispatcher().getPackedLightCoords(entity, partialTick));
      if (speechBubbleInstance == null) {
        continue;
      }

      ScreenRect screenRect =
          SpeechBubblePlacement.projectFacingRect(
              speechBubbleInstance.cameraSpaceRight(),
              speechBubbleInstance.cameraSpaceUp(),
              speechBubbleInstance.cameraSpaceDepth(),
              speechBubbleInstance.cameraSpaceHalfWidth(),
              speechBubbleInstance.cameraSpaceHalfHeight(),
              projectionScaleX,
              projectionScaleY);
      if (screenRect == null) {
        continue;
      }

      speechBubbleInstances.add(speechBubbleInstance);
      screenRects.add(screenRect);
    }

    if (speechBubbleInstances.isEmpty()) {
      return;
    }

    List<Integer> drawOrder = new ArrayList<>();
    for (int index = 0; index < speechBubbleInstances.size(); index++) {
      drawOrder.add(index);
    }
    drawOrder.sort(
        Comparator.comparingDouble(
                (Integer index) -> speechBubbleInstances.get(index).cameraSpaceDepth())
            .thenComparing(index -> speechBubbleInstances.get(index).uuid()));

    OverlapOffsets overlapOffsets = resolveOverlaps(drawOrder, screenRects, speechBubbleInstances);
    Vec3 cameraPosition = camera.getPosition();

    for (int sortedIndex = drawOrder.size() - 1; sortedIndex >= 0; sortedIndex--) {
      SpeechBubbleInstance speechBubbleInstance =
          speechBubbleInstances.get(drawOrder.get(sortedIndex));
      float[] offsets =
          smoothOffsets(
              speechBubbleInstance.uuid(),
              overlapOffsets.offsetsX()[sortedIndex],
              overlapOffsets.offsetsY()[sortedIndex]);
      Vec3 entityPosition = speechBubbleInstance.entityPosition();

      poseStack.pushPose();
      poseStack.translate(
          entityPosition.x - cameraPosition.x,
          entityPosition.y - cameraPosition.y,
          entityPosition.z - cameraPosition.z);
      SpeechBubbleRenderer.draw(
          speechBubbleInstance,
          poseStack,
          bufferSource,
          SpeechBubblePlacement.ndcToLocalX(
              offsets[0],
              speechBubbleInstance.cameraSpaceDepth(),
              projectionScaleX,
              speechBubbleInstance.bubbleScale()),
          SpeechBubblePlacement.ndcToLocalY(
              offsets[1],
              speechBubbleInstance.cameraSpaceDepth(),
              projectionScaleY,
              speechBubbleInstance.bubbleScale()));
      poseStack.popPose();
    }

    bufferSource.endLastBatch();
  }

  private static OverlapOffsets resolveOverlaps(
      List<Integer> drawOrder,
      List<ScreenRect> screenRects,
      List<SpeechBubbleInstance> speechBubbleInstances) {
    int count = drawOrder.size();
    if (!ClientSpeechBubbleConfig.OVERLAP_RESOLUTION_ENABLED || count < 2) {
      return new OverlapOffsets(new float[count], new float[count]);
    }

    float[] centersX = new float[count];
    float[] centersY = new float[count];
    float[] halfWidths = new float[count];
    float[] halfHeights = new float[count];
    boolean[] tieBreakPositive = new boolean[count];

    for (int sortedIndex = 0; sortedIndex < count; sortedIndex++) {
      ScreenRect screenRect = screenRects.get(drawOrder.get(sortedIndex));
      centersX[sortedIndex] = screenRect.centerX();
      centersY[sortedIndex] = screenRect.centerY();
      halfWidths[sortedIndex] = screenRect.halfWidth();
      halfHeights[sortedIndex] = screenRect.halfHeight();
      tieBreakPositive[sortedIndex] =
          speechBubbleInstances.get(drawOrder.get(sortedIndex)).uuid().getLeastSignificantBits()
              >= 0L;
    }

    return SpeechBubblePlacement.resolveOverlaps(
        centersX,
        centersY,
        halfWidths,
        halfHeights,
        tieBreakPositive,
        SpeechBubblePlacement.OVERLAP_GAP_NDC,
        SpeechBubblePlacement.OVERLAP_GAP_NDC,
        SpeechBubblePlacement.MAX_OVERLAP_PUSH_NDC,
        SpeechBubblePlacement.MAX_OVERLAP_PUSH_NDC,
        SpeechBubblePlacement.VIEWPORT_LIMIT_NDC,
        SpeechBubblePlacement.OVERLAP_PASSES);
  }

  private static float[] smoothOffsets(UUID uuid, float targetX, float targetY) {
    float[] offsets = smoothedOffsets.get(uuid);
    if (offsets == null) {
      offsets = new float[] {targetX, targetY};
      smoothedOffsets.put(uuid, offsets);
      return offsets;
    }

    offsets[0] = SpeechBubblePlacement.approach(offsets[0], targetX, OFFSET_SMOOTHING);
    offsets[1] = SpeechBubblePlacement.approach(offsets[1], targetY, OFFSET_SMOOTHING);
    return offsets;
  }

  private static boolean isSpeechBubbleVisible(Entity entity, LocalPlayer player) {
    if (!(entity instanceof EasyNPC<?> easyNPC)) {
      return false;
    }

    if (ItemUtils.isPlayerHoldingEasyNPCWand(player)
        && entity.distanceToSqr(player) <= NPC_WAND_RENDER_RANGE * NPC_WAND_RENDER_RANGE) {
      return true;
    }

    return !entity.isInvisible()
        && !entity.isInvisibleTo(player)
        && AttributeHandler.getOpacity(easyNPC) > DisplayAttributeType.MIN_OPACITY;
  }
}

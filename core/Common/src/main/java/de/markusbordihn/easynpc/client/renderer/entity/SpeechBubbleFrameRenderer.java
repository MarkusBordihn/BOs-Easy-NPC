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
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.SubmitNodeCollector;
import net.minecraft.client.renderer.state.level.CameraRenderState;
import net.minecraft.world.entity.Entity;

public class SpeechBubbleFrameRenderer {

  private static final float OFFSET_SMOOTHING = 0.25F;
  private static final float[] NO_OFFSET = new float[2];
  private static final Map<UUID, float[]> smoothedOffsets = new HashMap<>();
  private static final List<PreviousFrameEntry> previousFrameEntries = new ArrayList<>();
  private static final Set<UUID> previousFrameIdentifiers = new HashSet<>();

  private SpeechBubbleFrameRenderer() {}

  public static void clear() {
    smoothedOffsets.clear();
    previousFrameEntries.clear();
    previousFrameIdentifiers.clear();
  }

  public static void submit(
      Entity entity,
      PoseStack poseStack,
      SubmitNodeCollector submitNodeCollector,
      CameraRenderState cameraRenderState,
      int packedLight) {
    SpeechBubbleEntry speechBubbleEntry = SpeechBubbleManager.get(entity.getUUID());
    if (speechBubbleEntry == null) {
      return;
    }

    Minecraft minecraft = Minecraft.getInstance();
    if (!EasyNPCRenderVisibility.isVisibleTo(entity, minecraft.player)) {
      return;
    }

    SpeechBubbleInstance speechBubbleInstance =
        SpeechBubbleRenderer.measure(
            entity,
            speechBubbleEntry,
            minecraft,
            minecraft.getDeltaTracker().getGameTimeDeltaPartialTick(false),
            packedLight);
    if (speechBubbleInstance == null) {
      return;
    }

    float aspectRatio =
        minecraft.getWindow().getWidth() / (float) minecraft.getWindow().getHeight();
    float projectionScaleY =
        (float) (1.0D / Math.tan(Math.toRadians(minecraft.options.fov().get() / 2.0F)));
    float projectionScaleX = projectionScaleY / aspectRatio;
    float[] offsets = collect(speechBubbleInstance, projectionScaleX, projectionScaleY);

    SpeechBubbleRenderer.draw(
        speechBubbleInstance,
        poseStack,
        submitNodeCollector,
        cameraRenderState,
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
  }

  private static float[] collect(
      SpeechBubbleInstance speechBubbleInstance, float projectionScaleX, float projectionScaleY) {
    UUID uuid = speechBubbleInstance.uuid();
    if (!previousFrameIdentifiers.add(uuid)) {
      resolveCollectedFrame();
      previousFrameIdentifiers.add(uuid);
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
    if (screenRect != null) {
      previousFrameEntries.add(
          new PreviousFrameEntry(uuid, screenRect, speechBubbleInstance.cameraSpaceDepth()));
    }

    return smoothedOffsets.getOrDefault(uuid, NO_OFFSET);
  }

  private static void resolveCollectedFrame() {
    smoothedOffsets.keySet().removeIf(uuid -> SpeechBubbleManager.get(uuid) == null);
    previousFrameEntries.sort(
        Comparator.<PreviousFrameEntry>comparingDouble(PreviousFrameEntry::cameraSpaceDepth)
            .thenComparing(PreviousFrameEntry::uuid));

    OverlapOffsets overlapOffsets = resolveOverlaps(previousFrameEntries);
    for (int index = 0; index < previousFrameEntries.size(); index++) {
      smoothOffsets(
          previousFrameEntries.get(index).uuid(),
          overlapOffsets.offsetsX()[index],
          overlapOffsets.offsetsY()[index]);
    }

    previousFrameEntries.clear();
    previousFrameIdentifiers.clear();
  }

  private static OverlapOffsets resolveOverlaps(List<PreviousFrameEntry> entries) {
    int count = entries.size();
    if (!ClientSpeechBubbleConfig.OVERLAP_RESOLUTION_ENABLED || count < 2) {
      return new OverlapOffsets(new float[count], new float[count]);
    }

    float[] centersX = new float[count];
    float[] centersY = new float[count];
    float[] halfWidths = new float[count];
    float[] halfHeights = new float[count];
    boolean[] tieBreakPositive = new boolean[count];

    for (int index = 0; index < count; index++) {
      PreviousFrameEntry previousFrameEntry = entries.get(index);
      centersX[index] = previousFrameEntry.screenRect().centerX();
      centersY[index] = previousFrameEntry.screenRect().centerY();
      halfWidths[index] = previousFrameEntry.screenRect().halfWidth();
      halfHeights[index] = previousFrameEntry.screenRect().halfHeight();
      tieBreakPositive[index] = previousFrameEntry.uuid().getLeastSignificantBits() >= 0L;
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

  private static void smoothOffsets(UUID uuid, float targetX, float targetY) {
    float[] offsets = smoothedOffsets.get(uuid);
    if (offsets == null) {
      smoothedOffsets.put(uuid, new float[] {targetX, targetY});
      return;
    }

    offsets[0] = SpeechBubblePlacement.approach(offsets[0], targetX, OFFSET_SMOOTHING);
    offsets[1] = SpeechBubblePlacement.approach(offsets[1], targetY, OFFSET_SMOOTHING);
  }

  private record PreviousFrameEntry(UUID uuid, ScreenRect screenRect, double cameraSpaceDepth) {}
}

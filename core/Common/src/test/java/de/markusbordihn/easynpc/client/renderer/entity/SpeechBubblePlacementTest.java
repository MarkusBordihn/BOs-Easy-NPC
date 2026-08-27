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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import de.markusbordihn.easynpc.client.renderer.entity.SpeechBubblePlacement.OverlapOffsets;
import de.markusbordihn.easynpc.client.renderer.entity.SpeechBubblePlacement.ScreenRect;
import java.util.Arrays;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;

class SpeechBubblePlacementTest {

  private static final float CAMERA_EYE_HEIGHT = 1.62F;
  private static final float NAME_TAG_CLEARANCE = 0.3F;
  private static final float TAIL_HEIGHT = 8.0F;

  private static float anchorHeight(float entityHeight) {
    return entityHeight + 0.5F + NAME_TAG_CLEARANCE;
  }

  @Nested
  @DisplayName("Camera clearance")
  class CameraClearanceTests {

    @ParameterizedTest
    @DisplayName("Should clear the model half width whenever there is room for it")
    @CsvSource({
      "0.6, 32.0, 0.5",
      "0.6, 3.0, 0.5",
      "0.6, 1.0, 0.5",
      "0.6, 0.8, 0.3",
      "0.6, 0.5, 0.0",
      "0.6, 0.1, 0.0",
      "2.4, 8.0, 1.4"
    })
    void shouldClearTheModel(
        float entityWidth, double horizontalDistance, float expectedClearance) {
      assertEquals(
          expectedClearance,
          SpeechBubblePlacement.cameraClearance(entityWidth, horizontalDistance),
          1.0e-5F);
    }

    @ParameterizedTest
    @DisplayName("Should never move the bubble closer to the camera than half a block")
    @ValueSource(doubles = {0.0, 0.25, 0.5, 0.75, 1.2, 4.0, 64.0})
    void shouldKeepTheMinimumCameraDistance(double horizontalDistance) {
      float clearance = SpeechBubblePlacement.cameraClearance(0.6F, horizontalDistance);

      assertTrue(clearance >= 0.0F, "negative clearance at " + horizontalDistance);
      assertTrue(
          horizontalDistance - clearance >= Math.min(horizontalDistance, 0.5D) - 1.0e-5D,
          "bubble moved past the camera at " + horizontalDistance);
    }
  }

  @Nested
  @DisplayName("scaleForDistance")
  class ScaleForDistanceTests {

    @ParameterizedTest
    @DisplayName("Should keep the apparent size constant below the comfort distance")
    @CsvSource({
      "0.0, 0.0833333",
      "0.5, 0.0833333",
      "1.5, 0.25",
      "3.0, 0.5",
      "6.0, 1.0",
      "20.0, 1.0",
      "64.0, 1.0"
    })
    void shouldScaleWithDistance(double distance, float expectedBaseScaleRatio) {
      assertEquals(
          expectedBaseScaleRatio * SpeechBubblePlacement.BASE_SCALE,
          SpeechBubblePlacement.scaleForDistance(distance),
          1.0e-7F);
    }

    @Test
    @DisplayName("Should never exceed the base scale and never decrease with distance")
    void shouldStayMonotonicAndBounded() {
      float previousScale = 0.0F;
      for (double distance = 0.0D; distance <= 64.0D; distance += 0.25D) {
        float scale = SpeechBubblePlacement.scaleForDistance(distance);
        assertTrue(scale <= SpeechBubblePlacement.BASE_SCALE, "scale above base at " + distance);
        assertTrue(scale >= previousScale, "scale decreased at " + distance);
        previousScale = scale;
      }
    }
  }

  @Nested
  @DisplayName("View limits")
  class ViewLimitTests {

    @ParameterizedTest
    @DisplayName("Should derive the elevation limit from the field of view")
    @CsvSource({"30, 8.0", "70, 22.0", "90, 32.0", "110, 42.0"})
    void shouldDeriveElevationLimit(int fieldOfView, double expectedDegrees) {
      assertEquals(
          expectedDegrees,
          Math.toDegrees(SpeechBubblePlacement.elevationLimitRadians(fieldOfView)),
          1.0e-4D);
    }

    @ParameterizedTest
    @DisplayName("Should widen the azimuth limit with the aspect ratio")
    @CsvSource({"70, 1920, 1080, 41.2239", "70, 1080, 1080, 25.0", "30, 1920, 1080, 15.4710"})
    void shouldDeriveAzimuthLimit(
        int fieldOfView, int screenWidth, int screenHeight, double expectedDegrees) {
      assertEquals(
          expectedDegrees,
          Math.toDegrees(
              SpeechBubblePlacement.azimuthLimitRadians(fieldOfView, screenWidth, screenHeight)),
          1.0e-3D);
    }

    @Test
    @DisplayName("Should fall back to the minimum azimuth for a degenerate window size")
    void shouldHandleZeroScreenHeight() {
      assertEquals(
          15.0D, Math.toDegrees(SpeechBubblePlacement.azimuthLimitRadians(70, 1920, 0)), 1.0e-4D);
    }
  }

  @Nested
  @DisplayName("Anchor drop")
  class AnchorDropTests {

    @ParameterizedTest
    @DisplayName("Should only drop the anchor by the amount that exceeds the view limit")
    @CsvSource({"1.0, 2.0, 0.0", "2.424, 2.424, 0.0", "2.63, 2.424, 0.206", "3.5, 1.0, 2.5"})
    void shouldReturnTheExcess(
        double bubbleTopOffset, double maxVerticalOffset, float expectedDrop) {
      assertEquals(
          expectedDrop,
          SpeechBubblePlacement.requiredAnchorDrop(bubbleTopOffset, maxVerticalOffset),
          1.0e-5F);
    }

    @ParameterizedTest
    @DisplayName("Should never request a negative drop")
    @ValueSource(doubles = {-10.0, -0.5, 0.0, 0.5, 5.0})
    void shouldNeverBeNegative(double bubbleTopOffset) {
      assertTrue(SpeechBubblePlacement.requiredAnchorDrop(bubbleTopOffset, 2.0D) >= 0.0F);
    }
  }

  @Nested
  @DisplayName("Lateral progress")
  class LateralProgressTests {

    @ParameterizedTest
    @DisplayName("Should stay at zero until the drop saturates and reach one within half a block")
    @CsvSource({
      "0.0, 0.8, 0.0",
      "0.8, 0.8, 0.0",
      "0.9, 0.8, 0.2",
      "1.05, 0.8, 0.5",
      "1.3, 0.8, 1.0",
      "5.0, 0.8, 1.0"
    })
    void shouldBlendIntoTheLateralPlacement(
        float requiredDrop, float maxDrop, float expectedProgress) {
      assertEquals(
          expectedProgress, SpeechBubblePlacement.lateralProgress(requiredDrop, maxDrop), 1.0e-5F);
    }
  }

  @Nested
  @DisplayName("Fit scale")
  class FitScaleTests {

    @Test
    @DisplayName("Should not shrink the bubble while it fits into the view")
    void shouldKeepTheComfortScale() {
      assertEquals(
          0.025F, SpeechBubblePlacement.fitScale(0.025F, 7.1D, 5.2D, 66.0F, 86.0F), 1.0e-6F);
    }

    @Test
    @DisplayName("Should shrink the bubble to the tighter of both budgets")
    void shouldShrinkToTheTighterBudget() {
      assertEquals(
          0.0200F, SpeechBubblePlacement.fitScale(0.025F, 1.32D, 5.2D, 66.0F, 86.0F), 1.0e-6F);
      assertEquals(
          0.0200F, SpeechBubblePlacement.fitScale(0.025F, 7.1D, 1.72D, 66.0F, 86.0F), 1.0e-6F);
    }

    @ParameterizedTest
    @DisplayName("Should never shrink below the readability floor, even for a negative budget")
    @ValueSource(doubles = {-5.0, -0.1, 0.0, 0.01})
    void shouldRespectTheReadabilityFloor(double verticalBudget) {
      assertEquals(
          0.025F * 0.45F,
          SpeechBubblePlacement.fitScale(0.025F, verticalBudget, 5.2D, 66.0F, 86.0F),
          1.0e-6F);
    }

    @Test
    @DisplayName("Should not divide by a zero extent")
    void shouldHandleZeroExtents() {
      float scale = SpeechBubblePlacement.fitScale(0.025F, 1.0D, 1.0D, 0.0F, 0.0F);
      assertEquals(0.025F, scale, 1.0e-6F);
      assertFalse(Float.isNaN(scale));
    }
  }

  @Nested
  @DisplayName("Approach regression")
  class ApproachRegressionTests {

    @ParameterizedTest
    @DisplayName("Should keep a five line bubble within the elevation limit while approaching")
    @ValueSource(doubles = {0.6, 1.0, 1.5, 2.0, 3.0, 6.0, 12.0, 32.0})
    void shouldStayWithinTheElevationLimit(double horizontalDistance) {
      float entityHeight = 1.8F;
      float anchorHeight = anchorHeight(entityHeight);
      float maxAnchorDrop = anchorHeight - entityHeight;
      float extentAboveAnchor = TAIL_HEIGHT + 58.0F;

      double distanceToCamera = Math.hypot(horizontalDistance, anchorHeight - CAMERA_EYE_HEIGHT);
      float scale = SpeechBubblePlacement.scaleForDistance(distanceToCamera);
      float elevationLimitRadians = SpeechBubblePlacement.elevationLimitRadians(70);
      double maxVerticalOffset =
          SpeechBubblePlacement.maxOffsetWithinLimit(horizontalDistance, elevationLimitRadians);
      double bubbleTopOffset =
          anchorHeight - CAMERA_EYE_HEIGHT + extentAboveAnchor * (double) scale;

      float requiredDrop =
          SpeechBubblePlacement.requiredAnchorDrop(bubbleTopOffset, maxVerticalOffset);
      if (requiredDrop > maxAnchorDrop) {
        assertTrue(
            SpeechBubblePlacement.lateralProgress(requiredDrop, maxAnchorDrop) > 0.0F,
            "lateral placement should engage at " + horizontalDistance);
        return;
      }

      double elevation = Math.atan((bubbleTopOffset - requiredDrop) / horizontalDistance);
      assertTrue(
          elevation <= elevationLimitRadians + 1.0e-4D,
          "elevation " + Math.toDegrees(elevation) + "° at " + horizontalDistance + " blocks");
    }
  }

  @Nested
  @DisplayName("Lateral clearance")
  class LateralClearanceTests {

    @Test
    @DisplayName("Should clear the model half width even at the readability floor")
    void shouldClearTheModelAtTheSmallestScale() {
      float entityWidth = 0.6F;
      float scaleForDistance = 0.0058F;
      float clearancePixels =
          SpeechBubblePlacement.lateralClearancePixels(entityWidth, scaleForDistance);

      assertTrue(
          clearancePixels * scaleForDistance * 0.45F >= entityWidth * 0.5F - 1.0e-5F,
          "clearance below the model half width");
    }
  }

  @Nested
  @DisplayName("lerp")
  class LerpTests {

    @ParameterizedTest
    @CsvSource({"0.0, 2.6, 1.53, 2.6", "1.0, 2.6, 1.53, 1.53", "0.5, 2.6, 1.53, 2.065"})
    void shouldInterpolate(float progress, float from, float to, float expected) {
      assertEquals(expected, SpeechBubblePlacement.lerp(progress, from, to), 1.0e-5F);
    }
  }

  @Nested
  @DisplayName("Screen projection")
  class ProjectFacingRectTests {

    @Test
    @DisplayName("Should project a bubble straight ahead to the screen center")
    void shouldProjectToTheScreenCenter() {
      ScreenRect screenRect =
          SpeechBubblePlacement.projectFacingRect(0.0D, 0.0D, 4.0D, 0.5D, 0.25D, 1.5F, 2.0F);

      assertNotNull(screenRect);
      assertEquals(0.0F, screenRect.centerX(), 1.0e-5F);
      assertEquals(0.0F, screenRect.centerY(), 1.0e-5F);
    }

    @Test
    @DisplayName("Should halve the screen extent at double the depth")
    void shouldHalveTheExtentAtDoubleDepth() {
      ScreenRect nearRect =
          SpeechBubblePlacement.projectFacingRect(1.0D, 0.5D, 2.0D, 0.5D, 0.25D, 1.5F, 2.0F);
      ScreenRect farRect =
          SpeechBubblePlacement.projectFacingRect(1.0D, 0.5D, 4.0D, 0.5D, 0.25D, 1.5F, 2.0F);

      assertNotNull(nearRect);
      assertNotNull(farRect);
      assertEquals(nearRect.centerX() / 2.0F, farRect.centerX(), 1.0e-5F);
      assertEquals(nearRect.centerY() / 2.0F, farRect.centerY(), 1.0e-5F);
      assertEquals(nearRect.halfWidth() / 2.0F, farRect.halfWidth(), 1.0e-5F);
      assertEquals(nearRect.halfHeight() / 2.0F, farRect.halfHeight(), 1.0e-5F);
    }

    @ParameterizedTest
    @DisplayName("Should not project a bubble at or behind the near plane")
    @ValueSource(doubles = {-4.0, -0.1, 0.0, 0.049})
    void shouldNotProjectBehindTheCamera(double depth) {
      assertNull(
          SpeechBubblePlacement.projectFacingRect(1.0D, 0.5D, depth, 0.5D, 0.25D, 1.5F, 2.0F));
    }
  }

  @Nested
  @DisplayName("Push direction")
  class PushDirectionTests {

    @ParameterizedTest
    @CsvSource({"0.0, 0.4, 1.0", "0.0, -0.4, -1.0", "0.4, 0.0, -1.0", "-0.4, 0.0, 1.0"})
    void shouldFollowTheSignOutsideTheDeadBand(
        float nearCenter, float farCenter, float expectedDirection) {
      assertEquals(
          expectedDirection,
          SpeechBubblePlacement.pushDirection(nearCenter, farCenter, 0.02F, true),
          1.0e-5F);
    }

    @ParameterizedTest
    @DisplayName("Should use the tie break inside the dead band")
    @CsvSource({"true, 1.0", "false, -1.0"})
    void shouldUseTheTieBreak(boolean tieBreakPositive, float expectedDirection) {
      assertEquals(
          expectedDirection,
          SpeechBubblePlacement.pushDirection(0.5F, 0.51F, 0.02F, tieBreakPositive),
          1.0e-5F);
    }
  }

  @Nested
  @DisplayName("Overlap resolution")
  class ResolveOverlapsTests {

    private OverlapOffsets resolve(
        float[] centersX, float[] centersY, float maxPushY, float[] halfExtents) {
      boolean[] tieBreakPositive = new boolean[centersX.length];
      Arrays.fill(tieBreakPositive, true);

      return SpeechBubblePlacement.resolveOverlaps(
          centersX,
          centersY,
          halfExtents.clone(),
          halfExtents.clone(),
          tieBreakPositive,
          SpeechBubblePlacement.OVERLAP_GAP_NDC,
          SpeechBubblePlacement.OVERLAP_GAP_NDC,
          SpeechBubblePlacement.MAX_OVERLAP_PUSH_NDC,
          maxPushY,
          SpeechBubblePlacement.VIEWPORT_LIMIT_NDC,
          SpeechBubblePlacement.OVERLAP_PASSES);
    }

    @Test
    @DisplayName("Should leave separated bubbles untouched")
    void shouldLeaveSeparatedBubblesUntouched() {
      OverlapOffsets overlapOffsets =
          resolve(
              new float[] {-0.6F, 0.6F},
              new float[] {0.0F, 0.0F},
              SpeechBubblePlacement.MAX_OVERLAP_PUSH_NDC,
              new float[] {0.1F, 0.1F});

      assertEquals(0.0F, overlapOffsets.offsetsX()[1], 1.0e-5F);
      assertEquals(0.0F, overlapOffsets.offsetsY()[1], 1.0e-5F);
    }

    @Test
    @DisplayName("Should never move the nearest bubble")
    void shouldNeverMoveTheNearestBubble() {
      OverlapOffsets overlapOffsets =
          resolve(
              new float[] {0.0F, 0.0F, 0.0F},
              new float[] {0.0F, 0.0F, 0.0F},
              SpeechBubblePlacement.MAX_OVERLAP_PUSH_NDC,
              new float[] {0.1F, 0.1F, 0.1F});

      assertEquals(0.0F, overlapOffsets.offsetsX()[0], 1.0e-5F);
      assertEquals(0.0F, overlapOffsets.offsetsY()[0], 1.0e-5F);
    }

    @Test
    @DisplayName("Should separate two stacked bubbles by exactly the configured gap")
    void shouldSeparateTwoStackedBubbles() {
      float[] halfExtents = {0.1F, 0.1F};
      OverlapOffsets overlapOffsets =
          resolve(
              new float[] {0.0F, 0.0F},
              new float[] {0.0F, 0.0F},
              SpeechBubblePlacement.MAX_OVERLAP_PUSH_NDC,
              halfExtents);

      assertEquals(
          halfExtents[0] + halfExtents[1] + SpeechBubblePlacement.OVERLAP_GAP_NDC,
          Math.abs(overlapOffsets.offsetsY()[1]),
          1.0e-5F);
    }

    @Test
    @DisplayName("Should cascade three stacked bubbles without leaving an overlap")
    void shouldCascadeThreeStackedBubbles() {
      float[] halfExtents = {0.1F, 0.1F, 0.1F};
      float[] centersY = {0.0F, 0.0F, 0.0F};
      OverlapOffsets overlapOffsets =
          resolve(
              new float[] {0.0F, 0.0F, 0.0F},
              centersY,
              SpeechBubblePlacement.MAX_OVERLAP_PUSH_NDC,
              halfExtents);

      for (int index = 1; index < centersY.length; index++) {
        for (int otherIndex = 0; otherIndex < index; otherIndex++) {
          float distance =
              Math.abs(
                  centersY[index]
                      + overlapOffsets.offsetsY()[index]
                      - centersY[otherIndex]
                      - overlapOffsets.offsetsY()[otherIndex]);
          assertTrue(
              distance >= halfExtents[index] + halfExtents[otherIndex] - 1.0e-5F,
              "bubbles still overlap");
        }
      }
    }

    @Test
    @DisplayName("Should fall back to a sideways push once the vertical budget is spent")
    void shouldFallBackToASidewaysPush() {
      OverlapOffsets overlapOffsets =
          resolve(
              new float[] {0.0F, 0.0F},
              new float[] {0.0F, 0.0F},
              0.05F,
              new float[] {0.1F, 0.1F});

      assertEquals(0.05F, overlapOffsets.offsetsY()[1], 1.0e-5F);
      assertTrue(overlapOffsets.offsetsX()[1] > 0.0F, "no sideways push");
    }

    @Test
    @DisplayName("Should keep a pushed bubble inside the viewport")
    void shouldKeepThePushedBubbleInsideTheViewport() {
      float[] halfExtents = {0.1F, 0.1F};
      float[] centersY = {0.9F, 0.9F};
      OverlapOffsets overlapOffsets =
          resolve(
              new float[] {0.0F, 0.0F},
              centersY,
              SpeechBubblePlacement.MAX_OVERLAP_PUSH_NDC,
              halfExtents);

      assertTrue(
          centersY[1] + overlapOffsets.offsetsY()[1] + halfExtents[1]
              <= SpeechBubblePlacement.VIEWPORT_LIMIT_NDC + 1.0e-5F,
          "pushed beyond the viewport limit");
    }

    @Test
    @DisplayName("Should change the push direction at most once across a slow camera sweep")
    void shouldNotFlipDirectionRepeatedly() {
      int signChanges = 0;
      float previousSign = 0.0F;

      for (int step = 0; step <= 200; step++) {
        OverlapOffsets overlapOffsets =
            resolve(
                new float[] {0.0F, 0.0F},
                new float[] {0.0F, -0.05F + step * 0.0005F},
                SpeechBubblePlacement.MAX_OVERLAP_PUSH_NDC,
                new float[] {0.1F, 0.1F});
        float sign = Math.signum(overlapOffsets.offsetsY()[1]);
        if (sign == 0.0F) {
          continue;
        }

        if (previousSign != 0.0F && sign != previousSign) {
          signChanges++;
        }
        previousSign = sign;
      }

      assertTrue(signChanges <= 1, "direction flipped " + signChanges + " times");
    }
  }

  @Nested
  @DisplayName("Offset conversion")
  class OffsetConversionTests {

    @ParameterizedTest
    @DisplayName("Should convert a screen offset back into the same screen offset")
    @ValueSource(floats = {-0.5F, -0.012F, 0.0F, 0.08F, 0.5F})
    void shouldRoundTripScreenOffsets(float ndcOffset) {
      float localX = SpeechBubblePlacement.ndcToLocalX(ndcOffset, 6.0D, 1.5F, 0.03F);
      float localY = SpeechBubblePlacement.ndcToLocalY(ndcOffset, 6.0D, 2.0F, 0.03F);

      assertEquals(ndcOffset, (float) (localX * 0.03F * 1.5F / 6.0D), 1.0e-5F);
      assertEquals(ndcOffset, (float) (-localY * 0.03F * 2.0F / 6.0D), 1.0e-5F);
    }

    @ParameterizedTest
    @DisplayName("Should stay at zero for a degenerate projection or scale")
    @CsvSource({"0.0, 0.03", "1.5, 0.0"})
    void shouldStayAtZeroForDegenerateInput(float projectionScale, float bubbleScale) {
      assertEquals(
          0.0F, SpeechBubblePlacement.ndcToLocalX(0.4F, 6.0D, projectionScale, bubbleScale), 0.0F);
      assertEquals(
          0.0F, SpeechBubblePlacement.ndcToLocalY(0.4F, 6.0D, projectionScale, bubbleScale), 0.0F);
    }
  }

  @Nested
  @DisplayName("approach")
  class ApproachTests {

    @ParameterizedTest
    @CsvSource({
      "0.0, 1.0, 0.0, 0.0",
      "0.0, 1.0, 1.0, 1.0",
      "0.0, 1.0, 0.25, 0.25",
      "0.4, -0.4, 0.5, 0.0",
      "0.0, 1.0, 4.0, 1.0",
      "0.0, 1.0, -2.0, 0.0"
    })
    void shouldMoveTowardsTheTarget(float current, float target, float progress, float expected) {
      assertEquals(expected, SpeechBubblePlacement.approach(current, target, progress), 1.0e-5F);
    }
  }
}

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
import static org.junit.jupiter.api.Assertions.assertTrue;

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
}

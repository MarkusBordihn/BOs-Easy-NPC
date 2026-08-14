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

public final class SpeechBubblePlacement {

  public static final float BASE_SCALE = 0.03F;

  private static final float AZIMUTH_MARGIN_DEGREES = 10.0F;
  private static final float COMFORT_DISTANCE = 6.0F;
  private static final float ELEVATION_MARGIN_DEGREES = 13.0F;
  private static final float LATERAL_TRANSITION_BLOCKS = 0.5F;
  private static final float MIN_AZIMUTH_DEGREES = 15.0F;
  private static final float MIN_CAMERA_DISTANCE = 0.5F;
  private static final float MIN_EFFECTIVE_DISTANCE = 0.5F;
  private static final float MIN_ELEVATION_DEGREES = 8.0F;
  private static final float MIN_FIT_RATIO = 0.45F;
  private static final float MODEL_CLEARANCE_BLOCKS = 0.2F;

  private SpeechBubblePlacement() {}

  public static float cameraClearance(float entityWidth, double horizontalDistanceToCamera) {
    double clearance =
        Math.min(
            entityWidth * 0.5F + MODEL_CLEARANCE_BLOCKS,
            horizontalDistanceToCamera - MIN_CAMERA_DISTANCE);

    return (float) Math.max(0.0D, clearance);
  }

  public static float scaleForDistance(double distanceToCamera) {
    double effectiveDistance = Math.max(distanceToCamera, MIN_EFFECTIVE_DISTANCE);
    return BASE_SCALE * (float) Math.min(1.0D, effectiveDistance / COMFORT_DISTANCE);
  }

  public static float elevationLimitRadians(int fieldOfViewDegrees) {
    float limitDegrees =
        Math.max(MIN_ELEVATION_DEGREES, fieldOfViewDegrees / 2.0F - ELEVATION_MARGIN_DEGREES);
    return (float) Math.toRadians(limitDegrees);
  }

  public static float azimuthLimitRadians(
      int fieldOfViewDegrees, int screenWidth, int screenHeight) {
    if (screenHeight <= 0) {
      return (float) Math.toRadians(MIN_AZIMUTH_DEGREES);
    }

    double verticalHalfAngle = Math.toRadians(fieldOfViewDegrees / 2.0F);
    double horizontalHalfAngleDegrees =
        Math.toDegrees(
            Math.atan(Math.tan(verticalHalfAngle) * screenWidth / (double) screenHeight));
    return (float)
        Math.toRadians(
            Math.max(MIN_AZIMUTH_DEGREES, horizontalHalfAngleDegrees - AZIMUTH_MARGIN_DEGREES));
  }

  public static double maxOffsetWithinLimit(double distanceToCamera, float limitRadians) {
    return distanceToCamera * Math.tan(limitRadians);
  }

  public static float requiredAnchorDrop(
      double bubbleTopOffsetFromCamera, double maxVerticalOffset) {
    return (float) Math.max(0.0D, bubbleTopOffsetFromCamera - maxVerticalOffset);
  }

  public static float lateralProgress(float requiredAnchorDrop, float maxAnchorDrop) {
    if (requiredAnchorDrop <= maxAnchorDrop) {
      return 0.0F;
    }

    return Math.min(1.0F, (requiredAnchorDrop - maxAnchorDrop) / LATERAL_TRANSITION_BLOCKS);
  }

  public static float lateralClearancePixels(float entityWidth, float scaleForDistance) {
    return entityWidth * 0.5F / (scaleForDistance * MIN_FIT_RATIO);
  }

  public static float fitScale(
      float scaleForDistance,
      double verticalBudget,
      double horizontalBudget,
      float extentAboveAnchor,
      float extentAsideAnchor) {
    double verticalFit =
        extentAboveAnchor > 0.0F ? verticalBudget / extentAboveAnchor : Double.MAX_VALUE;
    double horizontalFit =
        extentAsideAnchor > 0.0F ? horizontalBudget / extentAsideAnchor : Double.MAX_VALUE;
    float fittedScale = (float) Math.min(verticalFit, horizontalFit);

    return Math.max(scaleForDistance * MIN_FIT_RATIO, Math.min(scaleForDistance, fittedScale));
  }

  public static float lerp(float progress, float from, float to) {
    return from + (to - from) * progress;
  }
}

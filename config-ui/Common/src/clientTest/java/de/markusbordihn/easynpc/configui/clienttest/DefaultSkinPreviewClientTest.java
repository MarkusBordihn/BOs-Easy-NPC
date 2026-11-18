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

package de.markusbordihn.easynpc.configui.clienttest;

import static org.junit.jupiter.api.Assertions.assertTrue;

import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.Until;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.JsonValue;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.Screenshot;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import javax.imageio.ImageIO;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class DefaultSkinPreviewClientTest extends ClientTestBase {

  private static final int PREVIEW_COUNT = 10;
  private static final int PREVIEWS_PER_ROW = 5;
  private static final int PREVIEW_WIDTH = 60;
  private static final int PREVIEW_HEIGHT = 84;
  private static final int DEFAULT_BUTTON_TO_FIRST_PREVIEW_X = -5;
  private static final int DEFAULT_BUTTON_TO_FIRST_PREVIEW_Y = 41;
  private static final int ENTITY_CROP_LEFT = 13;
  private static final int ENTITY_CROP_TOP = 14;
  private static final int ENTITY_CROP_WIDTH = 30;
  private static final int ENTITY_CROP_HEIGHT = 62;
  private static final int PIXEL_TOLERANCE = 32;
  private static final double MINIMUM_DISTINCT_PREVIEW_CHANGED_RATIO = 0.05;
  private static final int MINIMUM_DISTINCT_PREVIEWS = 8;
  private static final long PREVIEW_RENDER_TICKS = 10;

  private static List<GuiRegion> previewEntityRegions() {
    JsonValue defaultSkinButtonBounds =
        client.findWidgets(configurationButton("default")).stream()
            .map(widget -> widget.get("bounds"))
            .min(
                Comparator.comparingDouble((JsonValue bounds) -> bounds.number("y"))
                    .thenComparingDouble(bounds -> bounds.number("x")))
            .orElseThrow();
    int firstPreviewX =
        (int) defaultSkinButtonBounds.number("x") + DEFAULT_BUTTON_TO_FIRST_PREVIEW_X;
    int firstPreviewY =
        (int) defaultSkinButtonBounds.number("y") + DEFAULT_BUTTON_TO_FIRST_PREVIEW_Y;

    List<GuiRegion> previewRegions = new ArrayList<>();
    for (int i = 0; i < PREVIEW_COUNT; i++) {
      previewRegions.add(
          new GuiRegion(
              firstPreviewX + (i % PREVIEWS_PER_ROW) * PREVIEW_WIDTH + ENTITY_CROP_LEFT,
              firstPreviewY + (i / PREVIEWS_PER_ROW) * PREVIEW_HEIGHT + ENTITY_CROP_TOP,
              ENTITY_CROP_WIDTH,
              ENTITY_CROP_HEIGHT));
    }
    return previewRegions;
  }

  private static BufferedImage decode(Screenshot screenshot) {
    try {
      return ImageIO.read(new ByteArrayInputStream(screenshot.imageBytes()));
    } catch (IOException e) {
      throw new UncheckedIOException("Unable to decode screenshot", e);
    }
  }

  private static double changedRatio(
      BufferedImage image, double guiToImageScale, GuiRegion first, GuiRegion second) {
    int width = (int) (first.width() * guiToImageScale);
    int height = (int) (first.height() * guiToImageScale);
    int firstX = (int) (first.x() * guiToImageScale);
    int firstY = (int) (first.y() * guiToImageScale);
    int secondX = (int) (second.x() * guiToImageScale);
    int secondY = (int) (second.y() * guiToImageScale);
    long changedPixels = 0;
    for (int y = 0; y < height; y++) {
      for (int x = 0; x < width; x++) {
        if (differs(image.getRGB(firstX + x, firstY + y), image.getRGB(secondX + x, secondY + y))) {
          changedPixels++;
        }
      }
    }
    return (double) changedPixels / Math.max(1, (long) width * height);
  }

  private static boolean differs(int firstRGB, int secondRGB) {
    for (int shift = 0; shift <= 16; shift += 8) {
      if (Math.abs(((firstRGB >> shift) & 0xff) - ((secondRGB >> shift) & 0xff))
          > PIXEL_TOLERANCE) {
        return true;
      }
    }
    return false;
  }

  @Test
  @DisplayName("Default skin previews each render their own variant")
  void defaultSkinPreviewsRenderOwnVariant() {
    openMainConfiguration(spawnTestNpc());
    openSubPage("edit_skin", "default", "DefaultSkinConfigurationScreen");
    client.await(Until.ticksElapsed(PREVIEW_RENDER_TICKS));

    List<GuiRegion> previewRegions = previewEntityRegions();
    Screenshot screenshot = client.screenshot();
    client.saveScreenshot("default_skin_previews");
    BufferedImage image = decode(screenshot);
    double guiToImageScale = (double) image.getWidth() / screenshot.window().guiWidth();

    List<GuiRegion> distinctPreviews = new ArrayList<>();
    for (GuiRegion previewRegion : previewRegions) {
      boolean matchesEarlierPreview =
          distinctPreviews.stream()
              .anyMatch(distinctPreview ->
                  changedRatio(image, guiToImageScale, distinctPreview, previewRegion)
                      < MINIMUM_DISTINCT_PREVIEW_CHANGED_RATIO);
      if (!matchesEarlierPreview) {
        distinctPreviews.add(previewRegion);
      }
    }

    assertTrue(
        distinctPreviews.size() >= MINIMUM_DISTINCT_PREVIEWS,
        "Only "
            + distinctPreviews.size()
            + " of "
            + PREVIEW_COUNT
            + " default skin previews differ from each other");
  }

  private record GuiRegion(int x, int y, int width, int height) {}
}

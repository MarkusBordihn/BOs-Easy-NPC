/*
 * Copyright 2023 Markus Bordihn
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

package de.markusbordihn.easynpc.configui.client.screen.preset;

import de.markusbordihn.easynpc.client.screen.components.DrawBoxWithBorder;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.data.preset.PresetMetadata;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;

public class PresetInfoView {

  private static final float TEXT_SCALE = 0.8f;
  private static final SimpleDateFormat DATE_FORMAT = new SimpleDateFormat("yyyy-MM-dd HH:mm");

  private PresetInfoView() {
    // Utility class
  }

  public static void render(
      GuiGraphics guiGraphics,
      Font font,
      ResourceLocation preset,
      PresetMetadata metadata,
      int x,
      int y,
      int width,
      int height) {
    DrawBoxWithBorder.draw(guiGraphics, x, y, width, height);

    guiGraphics.pose().pushPose();
    guiGraphics.pose().scale(TEXT_SCALE, TEXT_SCALE, 1.0f);

    int scaledX = (int) ((x + 5) / TEXT_SCALE);
    int scaledY = (int) ((y + 3) / TEXT_SCALE);
    int scaledWidth = (int) ((width - 10) / TEXT_SCALE);
    int lineHeight = (int) (10 / TEXT_SCALE);
    int line = 0;

    Text.drawString(
        guiGraphics,
        font,
        Component.literal("Category: " + metadata.category() + " | Version: " + metadata.version()),
        scaledX,
        scaledY + lineHeight * line++,
        0x3F3F3F);

    if (!metadata.description().isEmpty()) {
      List<String> wrappedLines = Text.wrapText(font, metadata.description(), scaledWidth);
      for (int i = 0; i < Math.min(3, wrappedLines.size()); i++) {
        Text.drawString(
            guiGraphics,
            font,
            Component.literal(wrappedLines.get(i)),
            scaledX,
            scaledY + lineHeight * line++,
            0x3F3F3F);
      }
    }

    int scaledHeight = (int) ((height - 6) / TEXT_SCALE);
    int bottomY = (scaledY + scaledHeight - lineHeight) + 5;

    Text.drawString(
        guiGraphics,
        font,
        Component.literal(
            "by "
                + metadata.author()
                + " | created "
                + DATE_FORMAT.format(new Date(metadata.created()))),
        scaledX,
        bottomY,
        0x7F7F7F);

    guiGraphics.pose().popPose();
  }
}

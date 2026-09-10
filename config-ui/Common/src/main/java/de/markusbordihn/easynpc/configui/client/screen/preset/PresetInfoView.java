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

import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.configui.client.screen.components.DrawBoxWithBorder;
import de.markusbordihn.easynpc.data.preset.PresetMetadata;
import de.markusbordihn.easynpc.network.components.TextComponent;
import de.markusbordihn.easynpc.utils.TextUtils;
import de.markusbordihn.easynpc.utils.UUIDUtils;
import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.UUID;
import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.network.chat.Component;
import net.minecraft.world.phys.Vec3;

public class PresetInfoView {

  private static final float TEXT_SCALE = 0.8f;
  private static final String UNKNOWN_VALUE = "-";
  private static final int MAX_DESCRIPTION_LINES = 3;
  private static final DateTimeFormatter DATE_FORMAT =
      DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm").withZone(ZoneId.systemDefault());

  private PresetInfoView() {}

  public static void render(
      GuiGraphics guiGraphics,
      Font font,
      PresetListEntry entry,
      int x,
      int y,
      int width,
      int height) {
    DrawBoxWithBorder.draw(guiGraphics, x, y, width, height);

    int scaledX = x + 5;
    int scaledY = y + 3;
    int scaledWidth = width - 10;
    int lineHeight = 10;
    int maxLines = Math.max(1, ((height - 6) / 10) - 1);
    int line = 0;

    PresetMetadata metadata = entry.getMetadata();
    Text.drawString(
        guiGraphics,
        font,
        TextComponent.getTranslatedConfigText(
            "preset_browser.info.category", metadata.category(), metadata.version()),
        scaledX,
        scaledY + lineHeight * line++,
        0x3F3F3F);

    line = renderIdentity(guiGraphics, font, entry, scaledX, scaledY, lineHeight, line);

    if (!metadata.description().isEmpty()) {
      List<String> wrappedLines = Text.wrapText(font, metadata.description(), scaledWidth);
      int descriptionLines =
          Math.min(maxLines - line, Math.min(MAX_DESCRIPTION_LINES, wrappedLines.size()));
      for (int i = 0; i < descriptionLines; i++) {
        Text.drawString(
            guiGraphics,
            font,
            Component.literal(wrappedLines.get(i)),
            scaledX,
            scaledY + lineHeight * line++,
            0x3F3F3F);
      }
    }

    int scaledHeight = height - 6;
    Text.drawString(
        guiGraphics,
        font,
        TextComponent.getTranslatedConfigText(
            "preset_browser.info.author",
            metadata.author(),
            DATE_FORMAT.format(Instant.ofEpochMilli(metadata.created()))),
        scaledX,
        (scaledY + scaledHeight - lineHeight) + 3,
        0x7F7F7F);
  }

  private static int renderIdentity(
      GuiGraphics guiGraphics,
      Font font,
      PresetListEntry entry,
      int scaledX,
      int scaledY,
      int lineHeight,
      int line) {
    int currentLine = line;
    UUID entityUUID = entry.getStoredEntityUUID();
    if (entityUUID == null) {
      Text.drawString(
          guiGraphics,
          font,
          TextComponent.getTranslatedConfigText("preset_browser.identity_none"),
          scaledX,
          scaledY + lineHeight * currentLine++,
          0x7F7F7F);
      return currentLine;
    }

    Vec3 position = entry.getStoredPosition();
    String positionText = UNKNOWN_VALUE;
    if (position != null) {
      positionText = TextUtils.formatPosition(position);
    }

    Text.drawString(
        guiGraphics,
        font,
        TextComponent.getTranslatedConfigText(
            "preset_browser.identity", UUIDUtils.shortId(entityUUID), positionText),
        scaledX,
        scaledY + lineHeight * currentLine++,
        0x3F3F3F);

    UUID presetUUID = entry.getStoredPresetUUID();
    if (presetUUID != null) {
      Text.drawString(
          guiGraphics,
          font,
          TextComponent.getTranslatedConfigText(
              "preset_browser.identity_preset", UUIDUtils.shortId(presetUUID)),
          scaledX,
          scaledY + lineHeight * currentLine++,
          0x7F7F7F);
    }

    return currentLine;
  }
}

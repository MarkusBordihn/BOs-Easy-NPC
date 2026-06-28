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

package de.markusbordihn.easynpc.configui.client.screen.components;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import java.util.function.IntConsumer;
import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.GuiGraphicsExtractor;
import net.minecraft.client.input.CharacterEvent;
import net.minecraft.client.input.KeyEvent;
import net.minecraft.client.input.MouseButtonEvent;

public class ColorPickerPopup extends Popup {

  public static final int COLUMNS = 8;
  public static final int ROWS = 4;
  public static final int SWATCH_SIZE = 14;
  public static final int SWATCH_GAP = 2;
  public static final int[] PALETTE = createPalette();
  private static final int PADDING = 6;
  private static final int TITLE_HEIGHT = 11;
  private static final int FIELD_HEIGHT = 16;
  private static final int RGB_LABEL_WIDTH = 22;
  private static final int PREVIEW_SIZE = FIELD_HEIGHT;
  private static final int FIELD_X_OFFSET = PADDING + RGB_LABEL_WIDTH + PREVIEW_SIZE + 4;
  private static final int PANEL_WIDTH =
      PADDING * 2 + COLUMNS * SWATCH_SIZE + (COLUMNS - 1) * SWATCH_GAP;
  private static final int FIELD_WIDTH = PANEL_WIDTH - PADDING - FIELD_X_OFFSET;
  private static final int GRID_HEIGHT = ROWS * SWATCH_SIZE + (ROWS - 1) * SWATCH_GAP;
  private static final int PANEL_HEIGHT =
      PADDING * 2 + TITLE_HEIGHT + GRID_HEIGHT + PADDING + FIELD_HEIGHT;
  private final Font font;
  private final IntConsumer onColorSelected;
  private final TextField hexField;

  private boolean suppressResponder = false;
  private int gridLeft;
  private int gridTop;
  private int selectedColor;

  public ColorPickerPopup(Font font, IntConsumer onColorSelected) {
    this.font = font;
    this.onColorSelected = onColorSelected;
    this.hexField = new TextField(font, 0, 0, FIELD_WIDTH, FIELD_HEIGHT);
    this.hexField.setMaxLength(7);
    this.hexField.setResponder(value -> this.onHexFieldChanged());
  }

  private static int[] createPalette() {
    return new int[] {
      // Grayscale ramp.
      0xFFFFFF, 0xD4D4D4, 0xAAAAAA, 0x808080, 0x555555, 0x2B2B2B, 0x151515, 0x000000,
      // Vibrant hues.
      0xFF0000, 0xFF7F00, 0xFFFF00, 0x7FFF00, 0x00FF00, 0x00FFFF, 0x0000FF, 0xFF00FF,
      // Dark / muted tones.
      0x8B0000, 0xB5651D, 0x808000, 0x006400, 0x008080, 0x000080, 0x4B0082, 0x800080,
      // Pastel tones.
      0xFF9999, 0xFFCC99, 0xFFFF99, 0xCCFF99, 0x99FFCC, 0x99FFFF, 0x99CCFF, 0xFF99FF
    };
  }

  public void toggle(
      int currentColor, int anchorX, int anchorY, int screenWidth, int screenHeight) {
    if (this.isVisible()) {
      this.close();
    } else {
      this.selectedColor = currentColor & 0xffffff;
      this.open(anchorX, anchorY, screenWidth, screenHeight);
    }
  }

  @Override
  protected int getPanelWidth() {
    return PANEL_WIDTH;
  }

  @Override
  protected int getPanelHeight() {
    return PANEL_HEIGHT;
  }

  @Override
  protected void onOpen() {
    this.gridLeft = this.getX() + PADDING;
    this.gridTop = this.getY() + PADDING + TITLE_HEIGHT;
    this.hexField.setX(this.getX() + FIELD_X_OFFSET);
    this.hexField.setY(this.getY() + PADDING + TITLE_HEIGHT + GRID_HEIGHT + PADDING);
    this.setHexFieldValue(this.selectedColor);
    this.hexField.setFocused(false);
  }

  @Override
  protected void onClose() {
    this.hexField.setFocused(false);
  }

  private int swatchLeft(int index) {
    return this.gridLeft + (index % COLUMNS) * (SWATCH_SIZE + SWATCH_GAP);
  }

  private int swatchTop(int index) {
    return this.gridTop + (index / COLUMNS) * (SWATCH_SIZE + SWATCH_GAP);
  }

  private void setHexFieldValue(int color) {
    this.suppressResponder = true;
    this.hexField.setValue(ColorUtils.formatRgbColor(color));
    this.suppressResponder = false;
  }

  private void onHexFieldChanged() {
    if (this.suppressResponder) {
      return;
    }
    Integer color = ColorUtils.parseRgbColor(this.hexField.getValue());
    if (color != null) {
      this.selectedColor = color;
      this.onColorSelected.accept(color);
    }
  }

  private void selectColor(int color) {
    this.selectedColor = color & 0xffffff;
    this.setHexFieldValue(this.selectedColor);
    this.onColorSelected.accept(this.selectedColor);
  }

  @Override
  protected void renderContent(
      GuiGraphicsExtractor guiGraphics, int mouseX, int mouseY, float partialTicks) {
    Text.drawConfigStringShadow(
        guiGraphics,
        this.font,
        "color_picker.title",
        this.getX() + PADDING,
        this.getY() + PADDING,
        Constants.FONT_COLOR_WHITE);

    for (int index = 0; index < PALETTE.length; index++) {
      int swatchX = this.swatchLeft(index);
      int swatchY = this.swatchTop(index);
      int color = PALETTE[index];

      boolean hovered = contains(mouseX, mouseY, swatchX, swatchY, SWATCH_SIZE, SWATCH_SIZE);
      boolean selected = color == this.selectedColor;

      int borderColor = selected ? 0xFFFFFFFF : (hovered ? 0xFFFFFFAA : 0xFF000000);
      DrawBoxWithBorder.draw(
          guiGraphics, swatchX, swatchY, SWATCH_SIZE, SWATCH_SIZE, 0xFF000000 | color, borderColor);
    }

    Text.drawConfigString(
        guiGraphics,
        this.font,
        "color_picker.rgb",
        this.getX() + PADDING,
        this.hexField.getY() + (FIELD_HEIGHT - 8) / 2,
        Constants.FONT_COLOR_WHITE);

    Integer parsedColor = ColorUtils.parseRgbColor(this.hexField.getValue());
    int previewColor = parsedColor != null ? parsedColor : this.selectedColor;
    DrawBoxWithBorder.draw(
        guiGraphics,
        this.getX() + PADDING + RGB_LABEL_WIDTH,
        this.hexField.getY(),
        PREVIEW_SIZE,
        PREVIEW_SIZE,
        0xFF000000 | previewColor,
        0xFF000000);

    this.hexField.extractRenderState(guiGraphics, mouseX, mouseY, partialTicks);
  }

  @Override
  protected void onMouseClicked(MouseButtonEvent mouseButtonEvent, boolean doubleClick) {
    if (this.hexField.mouseClicked(mouseButtonEvent, doubleClick)) {
      this.hexField.setFocused(true);
      return;
    }
    this.hexField.setFocused(false);

    double mouseX = mouseButtonEvent.x();
    double mouseY = mouseButtonEvent.y();
    for (int index = 0; index < PALETTE.length; index++) {
      if (contains(
          mouseX,
          mouseY,
          this.swatchLeft(index),
          this.swatchTop(index),
          SWATCH_SIZE,
          SWATCH_SIZE)) {
        this.selectColor(PALETTE[index]);
        return;
      }
    }
  }

  @Override
  protected void onKeyPressed(KeyEvent keyEvent) {
    if (this.hexField.isFocused()) {
      this.hexField.keyPressed(keyEvent);
    }
  }

  @Override
  protected void onCharTyped(CharacterEvent characterEvent) {
    if (this.hexField.isFocused()) {
      this.hexField.charTyped(characterEvent);
    }
  }
}

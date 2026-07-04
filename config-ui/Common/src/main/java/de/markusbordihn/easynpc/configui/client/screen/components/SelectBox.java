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

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.AbstractWidget;
import net.minecraft.client.gui.narration.NarratedElementType;
import net.minecraft.client.gui.narration.NarrationElementOutput;
import net.minecraft.network.chat.Component;

public class SelectBox<T> extends AbstractWidget {

  private static final int MAX_VISIBLE_ENTRIES = 6;
  private static final int ENTRY_HEIGHT = 14;
  private static final int ARROW_WIDTH = 12;
  private static final int COLOR_BACKGROUND = 0xFF000000;
  private static final int COLOR_BORDER = 0xFFA0A0A0;
  private static final int COLOR_BORDER_INACTIVE = 0xFF707070;
  private static final int COLOR_ENTRY_HOVER = 0xFF404040;
  private static final int COLOR_TEXT = 0xFFE0E0E0;
  private static final int COLOR_TEXT_INACTIVE = 0xFF707070;
  private static final String NO_SELECTION_LABEL = "-";

  private final List<SelectOption<T>> options = new ArrayList<>();
  private final Consumer<T> onChange;

  private int selectedIndex = -1;
  private boolean open = false;

  public SelectBox(
      int left,
      int top,
      int width,
      int height,
      List<SelectOption<T>> options,
      Consumer<T> onChange) {
    super(left, top, width, height, Component.empty());
    this.options.addAll(options);
    this.onChange = onChange;
  }

  public void setOptions(List<SelectOption<T>> options) {
    this.options.clear();
    this.options.addAll(options);
    if (this.selectedIndex >= this.options.size()) {
      this.selectedIndex = -1;
    }
  }

  public T getSelectedValue() {
    return this.selectedIndex >= 0 && this.selectedIndex < this.options.size()
        ? this.options.get(this.selectedIndex).value()
        : null;
  }

  public void selectByValue(T value) {
    for (int index = 0; index < this.options.size(); index++) {
      T optionValue = this.options.get(index).value();
      if (optionValue != null && optionValue.equals(value)) {
        this.selectedIndex = index;
        return;
      }
    }
    this.selectedIndex = -1;
  }

  public boolean isOpen() {
    return this.open;
  }

  private int getVisibleEntries() {
    return Math.min(this.options.size(), MAX_VISIBLE_ENTRIES);
  }

  private int getDropdownTop() {
    return this.getY() + this.height;
  }

  private int getDropdownHeight() {
    return this.getVisibleEntries() * ENTRY_HEIGHT + 2;
  }

  private boolean isOverDropdown(double mouseX, double mouseY) {
    return this.open
        && mouseX >= this.getX()
        && mouseX < this.getX() + this.width
        && mouseY >= this.getDropdownTop()
        && mouseY < this.getDropdownTop() + this.getDropdownHeight();
  }

  @Override
  public boolean isMouseOver(double mouseX, double mouseY) {
    return super.isMouseOver(mouseX, mouseY) || this.isOverDropdown(mouseX, mouseY);
  }

  @Override
  public boolean mouseClicked(double mouseX, double mouseY, int button) {
    if (!this.active || !this.visible || button != 0) {
      return false;
    }

    if (this.isOverDropdown(mouseX, mouseY)) {
      int clickedIndex = (int) ((mouseY - this.getDropdownTop() - 1) / ENTRY_HEIGHT);
      if (clickedIndex >= 0 && clickedIndex < this.getVisibleEntries()) {
        this.selectedIndex = clickedIndex;
        this.open = false;
        this.playDownSound(Minecraft.getInstance().getSoundManager());
        if (this.onChange != null) {
          this.onChange.accept(this.options.get(clickedIndex).value());
        }
      }
      return true;
    }

    if (super.isMouseOver(mouseX, mouseY)) {
      this.open = !this.open && !this.options.isEmpty();
      this.playDownSound(Minecraft.getInstance().getSoundManager());
      return true;
    }

    this.open = false;
    return false;
  }

  @Override
  protected void renderWidget(GuiGraphics guiGraphics, int mouseX, int mouseY, float partialTicks) {
    Font font = Minecraft.getInstance().font;
    int borderColor = this.active ? COLOR_BORDER : COLOR_BORDER_INACTIVE;
    int textColor = this.active ? COLOR_TEXT : COLOR_TEXT_INACTIVE;

    // Field with current selection and drop-down arrow.
    DrawBoxWithBorder.draw(
        guiGraphics,
        this.getX(),
        this.getY(),
        this.width,
        this.height,
        COLOR_BACKGROUND,
        borderColor);
    String selectedLabel =
        this.selectedIndex >= 0 && this.selectedIndex < this.options.size()
            ? this.options.get(this.selectedIndex).label()
            : NO_SELECTION_LABEL;
    guiGraphics.drawString(
        font,
        font.plainSubstrByWidth(selectedLabel, this.width - ARROW_WIDTH - 8),
        this.getX() + 4,
        this.getY() + (this.height - 8) / 2,
        textColor,
        false);

    int arrowLeft = this.getX() + this.width - ARROW_WIDTH;
    guiGraphics.fill(
        arrowLeft, this.getY() + 1, arrowLeft + 1, this.getY() + this.height - 1, borderColor);
    int arrowCenterX = arrowLeft + ARROW_WIDTH / 2;
    int arrowTop = this.getY() + (this.height - 3) / 2;
    for (int row = 0; row < 3; row++) {
      guiGraphics.fill(
          arrowCenterX - 3 + row,
          arrowTop + row,
          arrowCenterX + 4 - row,
          arrowTop + row + 1,
          textColor);
    }

    if (!this.open) {
      return;
    }

    // Drop-down list rendered above sibling widgets.
    guiGraphics.pose().pushPose();
    guiGraphics.pose().translate(0, 0, 400);
    int dropdownTop = this.getDropdownTop();
    DrawBoxWithBorder.draw(
        guiGraphics,
        this.getX(),
        dropdownTop,
        this.width,
        this.getDropdownHeight(),
        COLOR_BACKGROUND,
        COLOR_BORDER);
    for (int index = 0; index < this.getVisibleEntries(); index++) {
      int entryTop = dropdownTop + 1 + index * ENTRY_HEIGHT;
      boolean entryHovered =
          mouseX >= this.getX()
              && mouseX < this.getX() + this.width
              && mouseY >= entryTop
              && mouseY < entryTop + ENTRY_HEIGHT;
      if (entryHovered || index == this.selectedIndex) {
        guiGraphics.fill(
            this.getX() + 1,
            entryTop,
            this.getX() + this.width - 1,
            entryTop + ENTRY_HEIGHT,
            entryHovered ? COLOR_ENTRY_HOVER : 0xFF303030);
      }
      guiGraphics.drawString(
          font,
          font.plainSubstrByWidth(this.options.get(index).label(), this.width - 10),
          this.getX() + 5,
          entryTop + (ENTRY_HEIGHT - 8) / 2,
          COLOR_TEXT,
          false);
    }
    guiGraphics.pose().popPose();
  }

  @Override
  protected void updateWidgetNarration(NarrationElementOutput narrationElementOutput) {
    narrationElementOutput.add(NarratedElementType.HINT, this.getMessage());
  }
}

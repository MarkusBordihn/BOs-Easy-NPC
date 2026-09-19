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

import de.markusbordihn.easynpc.client.screen.components.OverlayWidget;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.function.Consumer;
import java.util.function.Function;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.GuiGraphicsExtractor;
import net.minecraft.client.gui.components.AbstractWidget;
import net.minecraft.client.gui.narration.NarratedElementType;
import net.minecraft.client.gui.narration.NarrationElementOutput;
import net.minecraft.client.input.CharacterEvent;
import net.minecraft.client.input.KeyEvent;
import net.minecraft.client.input.MouseButtonEvent;
import net.minecraft.network.chat.Component;
import net.minecraft.util.Mth;
import org.lwjgl.glfw.GLFW;

public class SelectBox<T> extends AbstractWidget implements OverlayWidget {

  private static final int MAX_VISIBLE_ENTRIES = 6;
  private static final int SEARCH_THRESHOLD = 8;
  private static final int ENTRY_HEIGHT = 14;
  private static final int ARROW_WIDTH = 12;
  private static final int SCROLLBAR_WIDTH = 3;
  private static final int SEARCH_MAX_LENGTH = 128;
  private static final int COLOR_BACKGROUND = 0xFF000000;
  private static final int COLOR_BORDER = 0xFFA0A0A0;
  private static final int COLOR_BORDER_INACTIVE = 0xFF707070;
  private static final int COLOR_ENTRY_HOVER = 0xFF404040;
  private static final int COLOR_ENTRY_SELECTED = 0xFF303030;
  private static final int COLOR_SCROLLBAR = 0xFF808080;
  private static final int COLOR_TEXT = 0xFFE0E0E0;
  private static final int COLOR_TEXT_INACTIVE = 0xFF707070;
  private static final String NO_SELECTION_LABEL = "-";

  private final List<SelectOption<T>> options = new ArrayList<>();
  private final List<SelectOption<T>> filteredOptions = new ArrayList<>();
  private final Consumer<T> onChange;

  private Function<String, SelectOption<T>> customValueFactory;
  private TextField searchField;
  private Boolean searchable;
  private int selectedIndex = -1;
  private int scrollOffset = 0;
  private boolean open = false;
  private boolean openedDuringCurrentClick = false;

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

  private static boolean matchesAllSearchTerms(String label, String[] searchTerms) {
    for (String searchTerm : searchTerms) {
      if (!label.contains(searchTerm)) {
        return false;
      }
    }

    return true;
  }

  private static boolean containsLabel(List<? extends SelectOption<?>> options, String label) {
    for (SelectOption<?> option : options) {
      if (option.label().equalsIgnoreCase(label)) {
        return true;
      }
    }

    return false;
  }

  public void setOptions(List<SelectOption<T>> options) {
    this.options.clear();
    this.options.addAll(options);
    if (this.selectedIndex >= this.options.size()) {
      this.selectedIndex = -1;
    }
  }

  public void setCustomValueFactory(Function<String, SelectOption<T>> customValueFactory) {
    this.customValueFactory = customValueFactory;
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

  private boolean isSearchable() {
    return this.searchable != null ? this.searchable : this.options.size() > SEARCH_THRESHOLD;
  }

  public void setSearchable(boolean searchable) {
    this.searchable = searchable;
  }

  private int getVisibleEntries() {
    return Math.min(this.filteredOptions.size(), MAX_VISIBLE_ENTRIES);
  }

  private int getSearchHeight() {
    return this.isSearchable() ? ENTRY_HEIGHT : 0;
  }

  private int getDropdownHeight() {
    return this.getSearchHeight() + this.getVisibleEntries() * ENTRY_HEIGHT + 2;
  }

  private int getDropdownTop() {
    int dropdownBelow = this.getY() + this.height;
    int dropdownHeight = this.getDropdownHeight();
    if (dropdownBelow + dropdownHeight > Minecraft.getInstance().getWindow().getGuiScaledHeight()
        && this.getY() - dropdownHeight >= 0) {
      return this.getY() - dropdownHeight;
    }

    return dropdownBelow;
  }

  private int getEntriesTop() {
    return this.getDropdownTop() + 1 + this.getSearchHeight();
  }

  private int getMaxScrollOffset() {
    return Math.max(0, this.filteredOptions.size() - MAX_VISIBLE_ENTRIES);
  }

  private boolean isOverDropdown(double mouseX, double mouseY) {
    return this.open
        && mouseX >= this.getX()
        && mouseX < this.getX() + this.width
        && mouseY >= this.getDropdownTop()
        && mouseY < this.getDropdownTop() + this.getDropdownHeight();
  }

  private void openDropdown() {
    this.open = true;
    this.scrollOffset = 0;
    if (this.isSearchable()) {
      if (this.searchField == null) {
        this.searchField =
            new TextField(Minecraft.getInstance().font, 0, 0, this.width - 8, ENTRY_HEIGHT);
        this.searchField.setBordered(false);
        this.searchField.setMaxLength(SEARCH_MAX_LENGTH);
        this.searchField.setHint(Component.translatable("text.easy_npc.config.select_box.search"));
        this.searchField.setResponder(searchText -> this.applyFilter());
      }
      this.searchField.setValue("");
      this.searchField.setFocused(true);
    }
    this.applyFilter();
  }

  private void closeDropdown() {
    this.open = false;
    if (this.searchField != null) {
      this.searchField.setFocused(false);
    }
  }

  private void applyFilter() {
    this.filteredOptions.clear();
    String searchText = this.searchField != null ? this.searchField.getValue().trim() : "";
    if (searchText.isEmpty()) {
      this.filteredOptions.addAll(this.options);
    } else {
      String[] searchTerms = searchText.toLowerCase(Locale.ROOT).split("\\s+");
      for (SelectOption<T> option : this.options) {
        if (matchesAllSearchTerms(option.label().toLowerCase(Locale.ROOT), searchTerms)) {
          this.filteredOptions.add(option);
        }
      }
      if (this.customValueFactory != null && !containsLabel(this.filteredOptions, searchText)) {
        SelectOption<T> customOption = this.customValueFactory.apply(searchText);
        if (customOption != null) {
          this.filteredOptions.add(0, customOption);
        }
      }
    }
    this.scrollOffset = Mth.clamp(this.scrollOffset, 0, this.getMaxScrollOffset());
  }

  private void selectOption(SelectOption<T> option) {
    int optionIndex = this.options.indexOf(option);
    if (optionIndex < 0) {
      this.options.add(option);
      optionIndex = this.options.size() - 1;
    }

    this.selectedIndex = optionIndex;
    this.closeDropdown();
    this.playDownSound(Minecraft.getInstance().getSoundManager());
    if (this.onChange != null) {
      this.onChange.accept(option.value());
    }
  }

  @Override
  public boolean isMouseOver(double mouseX, double mouseY) {
    return super.isMouseOver(mouseX, mouseY) || this.isOverDropdown(mouseX, mouseY);
  }

  @Override
  public void setFocused(boolean focused) {
    super.setFocused(focused);
    boolean wasOpenedDuringCurrentClick = this.openedDuringCurrentClick;
    this.openedDuringCurrentClick = false;
    if (!focused && !wasOpenedDuringCurrentClick) {
      this.closeDropdown();
    }
  }

  @Override
  public boolean mouseClicked(MouseButtonEvent mouseButtonEvent, boolean doubleClick) {
    double mouseX = mouseButtonEvent.x();
    double mouseY = mouseButtonEvent.y();
    if (!this.active || !this.visible || mouseButtonEvent.button() != 0) {
      return false;
    }

    if (this.isOverDropdown(mouseX, mouseY)) {
      if (this.searchField != null && mouseY < this.getEntriesTop()) {
        this.searchField.setFocused(true);
        return true;
      }

      int clickedIndex = (int) ((mouseY - this.getEntriesTop()) / ENTRY_HEIGHT) + this.scrollOffset;
      if (clickedIndex >= 0 && clickedIndex < this.filteredOptions.size()) {
        this.selectOption(this.filteredOptions.get(clickedIndex));
      }
      return true;
    }

    if (super.isMouseOver(mouseX, mouseY)) {
      if (this.open) {
        this.closeDropdown();
      } else if (!this.options.isEmpty()) {
        this.openDropdown();
        this.openedDuringCurrentClick = true;
        this.playDownSound(Minecraft.getInstance().getSoundManager());
      }
      return true;
    }

    this.closeDropdown();
    return false;
  }

  @Override
  public boolean mouseScrolled(double mouseX, double mouseY, double scrollX, double scrollY) {
    if (!this.open || !this.isOverDropdown(mouseX, mouseY)) {
      return false;
    }

    this.scrollOffset =
        Mth.clamp(this.scrollOffset - (int) Math.signum(scrollY), 0, this.getMaxScrollOffset());
    return true;
  }

  @Override
  public boolean keyPressed(KeyEvent keyEvent) {
    if (!this.open) {
      return false;
    }

    if (keyEvent.input() == GLFW.GLFW_KEY_ESCAPE) {
      this.closeDropdown();
      return true;
    }

    return this.searchField != null && this.searchField.keyPressed(keyEvent);
  }

  @Override
  public boolean charTyped(CharacterEvent characterEvent) {
    return this.open && this.searchField != null && this.searchField.charTyped(characterEvent);
  }

  @Override
  protected void extractWidgetRenderState(
      GuiGraphicsExtractor guiGraphics, int mouseX, int mouseY, float partialTicks) {
    Font font = Minecraft.getInstance().font;
    int borderColor = this.active ? COLOR_BORDER : COLOR_BORDER_INACTIVE;
    int textColor = this.active ? COLOR_TEXT : COLOR_TEXT_INACTIVE;

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
    Text.drawString(
        guiGraphics,
        font,
        font.plainSubstrByWidth(selectedLabel, this.width - ARROW_WIDTH - 8),
        this.getX() + 4,
        this.getY() + (this.height - 8) / 2,
        textColor);

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
  }

  @Override
  public boolean hasOverlay() {
    return this.open && this.visible;
  }

  @Override
  public void renderOverlay(
      GuiGraphicsExtractor guiGraphics, int mouseX, int mouseY, float partialTicks) {
    this.renderDropdown(guiGraphics, Minecraft.getInstance().font, mouseX, mouseY, partialTicks);
  }

  private void renderDropdown(
      GuiGraphicsExtractor guiGraphics, Font font, int mouseX, int mouseY, float partialTicks) {
    int dropdownTop = this.getDropdownTop();
    DrawBoxWithBorder.draw(
        guiGraphics,
        this.getX(),
        dropdownTop,
        this.width,
        this.getDropdownHeight(),
        COLOR_BACKGROUND,
        COLOR_BORDER);

    if (this.searchField != null) {
      this.searchField.setX(this.getX() + 5);
      this.searchField.setY(dropdownTop + 4);
      this.searchField.extractRenderState(guiGraphics, mouseX, mouseY, partialTicks);
      guiGraphics.fill(
          this.getX() + 1,
          dropdownTop + this.getSearchHeight(),
          this.getX() + this.width - 1,
          dropdownTop + this.getSearchHeight() + 1,
          COLOR_BORDER);
    }

    int entriesTop = this.getEntriesTop();
    for (int row = 0; row < this.getVisibleEntries(); row++) {
      int entryTop = entriesTop + row * ENTRY_HEIGHT;
      SelectOption<T> option = this.filteredOptions.get(row + this.scrollOffset);
      boolean entryHovered =
          mouseX >= this.getX()
              && mouseX < this.getX() + this.width
              && mouseY >= entryTop
              && mouseY < entryTop + ENTRY_HEIGHT;
      boolean entrySelected =
          this.selectedIndex >= 0
              && this.selectedIndex < this.options.size()
              && this.options.get(this.selectedIndex).equals(option);
      if (entryHovered || entrySelected) {
        guiGraphics.fill(
            this.getX() + 1,
            entryTop,
            this.getX() + this.width - 1,
            entryTop + ENTRY_HEIGHT,
            entryHovered ? COLOR_ENTRY_HOVER : COLOR_ENTRY_SELECTED);
      }
      Text.drawString(
          guiGraphics,
          font,
          font.plainSubstrByWidth(option.label(), this.width - 10),
          this.getX() + 5,
          entryTop + (ENTRY_HEIGHT - 8) / 2,
          COLOR_TEXT);
    }

    this.renderScrollbar(guiGraphics, entriesTop);
  }

  private void renderScrollbar(GuiGraphicsExtractor guiGraphics, int entriesTop) {
    if (this.filteredOptions.size() <= MAX_VISIBLE_ENTRIES) {
      return;
    }

    int trackHeight = MAX_VISIBLE_ENTRIES * ENTRY_HEIGHT;
    int handleHeight =
        Math.max(ENTRY_HEIGHT, trackHeight * MAX_VISIBLE_ENTRIES / this.filteredOptions.size());
    int handleTop =
        entriesTop + (trackHeight - handleHeight) * this.scrollOffset / this.getMaxScrollOffset();
    int handleLeft = this.getX() + this.width - SCROLLBAR_WIDTH - 1;
    guiGraphics.fill(
        handleLeft,
        handleTop,
        handleLeft + SCROLLBAR_WIDTH,
        handleTop + handleHeight,
        COLOR_SCROLLBAR);
  }

  @Override
  protected void updateWidgetNarration(NarrationElementOutput narrationElementOutput) {
    narrationElementOutput.add(NarratedElementType.HINT, this.getMessage());
  }
}

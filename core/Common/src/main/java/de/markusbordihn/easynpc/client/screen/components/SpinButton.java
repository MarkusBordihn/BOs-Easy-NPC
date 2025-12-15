/*
 * Copyright 2023 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software
 * and associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
 * BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.client.screen.components;

import de.markusbordihn.easynpc.network.components.TextComponent;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;

public class SpinButton<T> extends CustomButton {

  private final TextButton previousButton;
  private final TextButton nextButton;
  private final TextButton textButton;
  private final List<T> values;
  private final OnChange onChange;
  private int currentIndex;

  public SpinButton(
      int x, int y, int width, int height, Set<T> values, T initialValue, OnChange onChange) {
    super(x, y, width, height);
    this.values = new ArrayList<>(values);
    this.currentIndex = Math.max(0, this.values.indexOf(initialValue));
    this.onChange = onChange;

    int navWidth = 10;
    this.previousButton = new TextButton(x, y, navWidth, height, "<", this::previous);
    this.textButton = new TextButton(x + navWidth, y, width - 2 * navWidth, height, "", this::next);
    this.nextButton = new TextButton(x + width - navWidth, y, navWidth, height, ">", this::next);

    updateButtonStates();
  }

  private void previous(Button button) {
    if (this.currentIndex > 0) {
      setIndex(this.currentIndex - 1);
    }
  }

  private void next(Button button) {
    if (this.currentIndex < this.values.size() - 1) {
      setIndex(this.currentIndex + 1);
    } else if (button == this.textButton && !this.values.isEmpty()) {
      setIndex(0);
    }
  }

  private void setIndex(int index) {
    this.currentIndex = index;
    updateButtonStates();
    if (this.onChange != null) {
      this.onChange.onChange(this);
    }
  }

  private void updateButtonStates() {
    boolean hasValues = !this.values.isEmpty();
    this.previousButton.active = hasValues && this.currentIndex > 0;
    this.nextButton.active = hasValues && this.currentIndex < this.values.size() - 1;
    this.textButton.active = hasValues;
  }

  public T get() {
    return this.values.isEmpty() ? null : this.values.get(this.currentIndex);
  }

  @Override
  public void renderButton(GuiGraphics guiGraphics, int left, int top, float partialTicks) {
    this.previousButton.renderButton(guiGraphics, left, top, partialTicks);
    this.nextButton.renderButton(guiGraphics, left, top, partialTicks);
    this.textButton.renderButton(guiGraphics, left, top, partialTicks);

    T value = get();
    if (value != null) {
      this.textButton.setMessage(TextComponent.getText(value.toString()));
    }
  }

  @Override
  public boolean mouseClicked(double mouseX, double mouseY, int button) {
    return this.previousButton.mouseClicked(mouseX, mouseY, button)
        || this.nextButton.mouseClicked(mouseX, mouseY, button)
        || this.textButton.mouseClicked(mouseX, mouseY, button);
  }

  public interface OnChange {
    void onChange(SpinButton<?> spinButton);
  }
}

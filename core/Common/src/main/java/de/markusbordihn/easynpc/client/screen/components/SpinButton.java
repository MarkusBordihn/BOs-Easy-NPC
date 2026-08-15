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
import java.util.function.Function;
import net.minecraft.client.gui.GuiGraphicsExtractor;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.input.MouseButtonEvent;
import net.minecraft.network.chat.Component;

public class SpinButton<T> extends CustomButton {

  private final TextButton previousButton;
  private final TextButton nextButton;
  private final TextButton textButton;
  private final List<T> values = new ArrayList<>();
  private final OnChange<T> onChange;
  private Function<T, Component> labelProvider = value -> TextComponent.getText(value.toString());
  private int currentIndex;

  public SpinButton(
      int x, int y, int width, int height, Set<T> values, T initialValue, OnChange<T> onChange) {
    super(x, y, width, height);
    this.values.addAll(values);
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

  public void setLabelProvider(Function<T, Component> labelProvider) {
    this.labelProvider = labelProvider;
  }

  public void setValues(Set<T> values, T selectedValue) {
    this.values.clear();
    this.values.addAll(values);
    this.currentIndex = Math.max(0, this.values.indexOf(selectedValue));
    updateButtonStates();
  }

  @Override
  public void renderButton(
      GuiGraphicsExtractor guiGraphics, int left, int top, float partialTicks) {
    this.previousButton.renderButton(guiGraphics, left, top, partialTicks);
    this.nextButton.renderButton(guiGraphics, left, top, partialTicks);
    this.textButton.renderButton(guiGraphics, left, top, partialTicks);

    T value = get();
    if (value != null) {
      this.textButton.setMessage(this.labelProvider.apply(value));
    }
  }

  @Override
  public boolean mouseClicked(MouseButtonEvent mouseButtonEvent, boolean doubleClick) {
    if (!this.visible || !this.active) {
      return false;
    }

    return this.previousButton.mouseClicked(mouseButtonEvent, doubleClick)
        || this.nextButton.mouseClicked(mouseButtonEvent, doubleClick)
        || this.textButton.mouseClicked(mouseButtonEvent, doubleClick);
  }

  @Override
  public void setY(int y) {
    super.setY(y);
    this.previousButton.setY(y);
    this.textButton.setY(y);
    this.nextButton.setY(y);
  }

  @Override
  public void setX(int x) {
    super.setX(x);
    int navWidth = 10;
    this.previousButton.setX(x);
    this.textButton.setX(x + navWidth);
    this.nextButton.setX(x + this.width - navWidth);
  }

  public interface OnChange<T> {
    void onChange(SpinButton<T> spinButton);
  }
}

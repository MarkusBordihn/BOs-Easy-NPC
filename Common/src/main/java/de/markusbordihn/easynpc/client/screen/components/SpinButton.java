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

import java.util.LinkedHashSet;
import java.util.Set;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.network.chat.Component;

public class SpinButton<T> extends CustomButton {

  private final TextButton previousButton;
  private final TextButton nextButton;
  private final TextButton textButton;
  private final Set<T> values = new LinkedHashSet<>();
  private final T defaultValue;
  private final OnChange onChange;
  private T currentValue;

  public SpinButton(
      int x, int y, int width, int height, Set<T> values, T initialValue, OnChange onChange) {
    super(x, y, width, height);
    this.values.addAll(values);
    this.defaultValue = initialValue;
    this.currentValue = initialValue;
    this.onChange = onChange;

    int navigationButtonWidth = 10;
    this.previousButton =
        new TextButton(x, y, navigationButtonWidth, height, "<", this::changeToPreviousValue);
    this.textButton = new TextButton(x + navigationButtonWidth, y, width, height);
    this.nextButton =
        new TextButton(
            x + width + navigationButtonWidth,
            y,
            navigationButtonWidth,
            height,
            ">",
            this::changeToNextValue);
  }

  private void changeToPreviousValue(Button button) {
    T previousValue = null;
    for (T value : this.values) {
      if (value.equals(this.currentValue)) {
        break;
      }
      previousValue = value;
    }
    if (previousValue != null) {
      this.set(previousValue);
    }
  }

  private void changeToNextValue(Button button) {
    T nextValue = null;
    boolean found = false;
    for (T value : this.values) {
      if (found) {
        nextValue = value;
        break;
      }
      if (value.equals(this.currentValue)) {
        found = true;
      }
    }
    if (nextValue != null) {
      this.set(nextValue);
    }
  }

  public void reset() {
    this.currentValue = this.defaultValue;
  }

  public void set(T value) {
    this.currentValue = value;
    if (this.onChange != null) {
      this.onChange.onChange(this);
    }
  }

  public T get() {
    return this.currentValue;
  }

  @Override
  public void renderButton(GuiGraphics guiGraphics, int left, int top, float partialTicks) {
    this.previousButton.renderButton(guiGraphics, left, top, partialTicks);
    this.nextButton.renderButton(guiGraphics, left, top, partialTicks);
    this.textButton.renderButton(guiGraphics, left, top, partialTicks);
    this.textButton.setMessage(Component.literal(this.currentValue.toString()));
  }

  @Override
  public boolean mouseClicked(double mouseX, double mouseY, int button) {
    if (this.previousButton.mouseClicked(mouseX, mouseY, button)) {
      return true;
    }
    return this.nextButton.mouseClicked(mouseX, mouseY, button);
  }

  public interface OnChange {
    void onChange(SpinButton<?> spinButton);
  }
}

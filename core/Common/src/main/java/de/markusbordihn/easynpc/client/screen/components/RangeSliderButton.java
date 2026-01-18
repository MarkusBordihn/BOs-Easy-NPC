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

package de.markusbordihn.easynpc.client.screen.components;

import de.markusbordihn.easynpc.client.screen.components.SliderButton.Type;
import de.markusbordihn.easynpc.network.components.TextComponent;
import de.markusbordihn.easynpc.utils.ValueUtils;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.AbstractWidget;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.narration.NarratedElementType;
import net.minecraft.client.gui.narration.NarrationElementOutput;
import net.minecraft.client.input.CharacterEvent;
import net.minecraft.client.input.KeyEvent;
import net.minecraft.client.input.MouseButtonEvent;
import net.minecraft.network.chat.Component;

public class RangeSliderButton extends AbstractWidget {

  public static final Component DECREASE_TEXT = TextComponent.getText("-");
  public static final Component INCREASE_TEXT = TextComponent.getText("+");
  public static final Component RESET_TEXT = TextComponent.getText("↺");
  public static final Component EDIT_TEXT = TextComponent.getText("✎");
  public static final Component DONE_TEXT = TextComponent.getText("✔");
  private static final int DEFAULT_WIDTH = 170;
  private static final int DEFAULT_HEIGHT = 14;
  private static final int DECREASE_BUTTON_WIDTH = 12;
  private static final int INCREASE_BUTTON_WIDTH = 12;
  private static final int RESET_BUTTON_WIDTH = 12;
  private static final int EDIT_BUTTON_WIDTH = 12;
  private final SliderButton sliderButton;
  private final TextButton textButtonDecrease;
  private final TextButton textButtonIncrease;
  private final TextButton textButtonReset;
  private final TextButton textButtonEdit;
  private final TextButton textButtonDone;
  private final TextField textField;
  private final SliderButton.Type sliderType;
  private final boolean showButtons;

  public RangeSliderButton(
      int left,
      int top,
      double value,
      double minValue,
      double maxValue,
      double defaultValue,
      double stepSize,
      SliderButton.OnChange onChange) {
    this(
        left,
        top,
        DEFAULT_WIDTH,
        DEFAULT_HEIGHT,
        value,
        minValue,
        maxValue,
        defaultValue,
        stepSize,
        onChange);
  }

  public RangeSliderButton(
      int left,
      int top,
      int width,
      int height,
      double value,
      double defaultValue,
      SliderButton.Type sliderType,
      SliderButton.OnChange onChange) {
    this(
        left,
        top,
        width,
        height,
        value,
        SliderButton.getMinValue(sliderType),
        SliderButton.getMaxValue(sliderType),
        defaultValue,
        SliderButton.getStepSize(sliderType),
        onChange);
  }

  public RangeSliderButton(
      int left,
      int top,
      int width,
      int height,
      double value,
      double defaultValue,
      SliderButton.Type sliderType,
      boolean showButtons,
      SliderButton.OnChange onChange) {
    this(
        left,
        top,
        width,
        height,
        value,
        SliderButton.getMinValue(sliderType),
        SliderButton.getMaxValue(sliderType),
        defaultValue,
        SliderButton.getStepSize(sliderType),
        sliderType,
        showButtons,
        onChange);
  }

  public RangeSliderButton(
      int left,
      int top,
      int width,
      int height,
      double value,
      double minValue,
      double maxValue,
      double defaultValue,
      double stepSize,
      SliderButton.OnChange onChange) {
    this(
        left,
        top,
        width,
        height,
        value,
        minValue,
        maxValue,
        defaultValue,
        stepSize,
        Type.DOUBLE,
        true,
        onChange);
  }

  public RangeSliderButton(
      int left,
      int top,
      int width,
      int height,
      double value,
      double minValue,
      double maxValue,
      double defaultValue,
      double stepSize,
      SliderButton.Type sliderType,
      boolean showButtons,
      SliderButton.OnChange onChange) {
    super(left, top, width, height, TextComponent.getBlankText());
    Font font = Minecraft.getInstance().font;
    this.sliderType = sliderType;
    this.showButtons = showButtons;
    this.sliderButton =
        new SliderButton(
            this.showButtons ? left + DECREASE_BUTTON_WIDTH : left,
            top,
            this.getDefaultSliderWidth(),
            height,
            value,
            minValue,
            maxValue,
            button -> updateSliderValue(button, onChange),
            this.sliderType);
    this.textField =
        switch (this.sliderType) {
          case DEGREE ->
              new DegreeNumberField(
                  font,
                  this.showButtons ? left + DECREASE_BUTTON_WIDTH : left,
                  top,
                  this.getDefaultSliderWidth(),
                  height,
                  value);
          case POSITION ->
              new PositionNumberField(
                  font,
                  this.showButtons ? left + DECREASE_BUTTON_WIDTH : left,
                  top,
                  this.getDefaultSliderWidth(),
                  height,
                  value,
                  minValue,
                  maxValue);
          case SCALE ->
              new ScaleNumberField(
                  font,
                  this.showButtons ? left + DECREASE_BUTTON_WIDTH : left,
                  top,
                  this.getDefaultSliderWidth(),
                  height,
                  value,
                  minValue,
                  maxValue);
          default ->
              new PositiveNumberField(
                  font,
                  this.showButtons ? left + DECREASE_BUTTON_WIDTH : left,
                  top,
                  this.getDefaultSliderWidth(),
                  height,
                  value);
        };
    this.textField.setResponder(
        text -> {
          if (ValueUtils.isDoubleValue(text, minValue, maxValue)) {
            this.sliderButton.setDefaultValue(Double.parseDouble(text));
          }
        });
    this.textButtonDecrease =
        new TextButton(
            this.sliderButton.getX() - DECREASE_BUTTON_WIDTH,
            top,
            DECREASE_BUTTON_WIDTH,
            height,
            DECREASE_TEXT,
            button -> {
              if (this.sliderButton.getTargetDoubleValue() - stepSize >= minValue) {
                this.sliderButton.setDefaultValue(
                    this.sliderButton.getTargetDoubleValue() - stepSize);
                this.updateTextField();
              }
            });
    this.textButtonDecrease.active = showButtons;
    this.textButtonIncrease =
        new TextButton(
            this.sliderButton.getX() + this.sliderButton.getWidth(),
            top,
            INCREASE_BUTTON_WIDTH,
            height,
            INCREASE_TEXT,
            button -> {
              if (this.sliderButton.getTargetDoubleValue() + stepSize <= maxValue) {
                this.sliderButton.setDefaultValue(
                    this.sliderButton.getTargetDoubleValue() + stepSize);
                this.updateTextField();
              }
            });
    this.textButtonIncrease.active = showButtons;
    this.textButtonReset =
        new TextButton(
            this.textButtonIncrease.getX() + this.textButtonIncrease.getWidth(),
            top,
            RESET_BUTTON_WIDTH,
            height,
            RESET_TEXT,
            button -> {
              this.sliderButton.setDefaultValue(defaultValue);
              this.updateTextField();
            });
    this.textButtonReset.active = showButtons;
    this.textButtonEdit =
        new TextButton(
            this.textButtonReset.getX() + this.textButtonReset.getWidth(),
            top,
            EDIT_BUTTON_WIDTH,
            height,
            EDIT_TEXT,
            this::showTextField);
    this.textButtonEdit.active = showButtons;
    this.textButtonDone =
        new TextButton(
            this.textButtonReset.getX() + this.textButtonReset.getWidth(),
            top,
            EDIT_BUTTON_WIDTH,
            height,
            DONE_TEXT,
            this::showSliderButton);
    this.textButtonDone.active = showButtons;
  }

  public void reset() {
    this.sliderButton.reset();
    this.updateTextField();
  }

  public int getDefaultSliderWidth() {
    if (this.showButtons) {
      return this.width
          - (DECREASE_BUTTON_WIDTH
              + INCREASE_BUTTON_WIDTH
              + RESET_BUTTON_WIDTH
              + EDIT_BUTTON_WIDTH);
    }
    return this.width;
  }

  private void updateSliderValue(SliderButton sliderButton, SliderButton.OnChange onChange) {
    onChange.onChange(sliderButton);
  }

  private void updateTextField() {
    String sliderValue =
        switch (this.sliderType) {
          case DEGREE ->
              String.format("%.1f", this.sliderButton.getTargetDoubleValue()).replace(",", ".");
          default ->
              String.format("%.2f", this.sliderButton.getTargetDoubleValue()).replace(",", ".");
        };
    if (!this.textField.getValue().equals(sliderValue)) {
      this.textField.setValue(sliderValue);
    }
  }

  public void showTextField() {
    this.showTextField(null);
  }

  public void showSliderButton() {
    this.showSliderButton(null);
  }

  private void showTextField(Button button) {
    this.updateTextField();
    this.sliderButton.visible = false;
    this.sliderButton.setFocused(false);
    this.textButtonEdit.visible = false;
    this.textButtonDone.visible = true;
    this.textField.visible = true;
  }

  private void showSliderButton(Button button) {
    this.sliderButton.visible = true;
    this.textButtonEdit.visible = true;
    this.textButtonDone.visible = false;
    this.textField.visible = false;
    this.textField.setFocused(false);
  }

  public float getTargetValue() {
    return this.sliderButton.getTargetValue();
  }

  public void setTargetValue(final float newValue) {
    this.sliderButton.setDefaultValue(newValue);
    this.updateTextField();
  }

  @Override
  public void renderWidget(GuiGraphics guiGraphics, int mouseX, int mouseY, float partialTicks) {
    if (sliderButton.visible) {
      sliderButton.render(guiGraphics, mouseX, mouseY, partialTicks);
    } else if (textField.visible) {
      textField.render(guiGraphics, mouseX, mouseY, partialTicks);
    }

    if (textButtonDecrease.active) {
      textButtonDecrease.render(guiGraphics, mouseX, mouseY, partialTicks);
    }
    if (textButtonIncrease.active) {
      textButtonIncrease.render(guiGraphics, mouseX, mouseY, partialTicks);
    }
    if (textButtonReset.active) {
      textButtonReset.render(guiGraphics, mouseX, mouseY, partialTicks);
    }

    if (textButtonEdit.active && textButtonEdit.visible) {
      textButtonEdit.render(guiGraphics, mouseX, mouseY, partialTicks);
    } else if (textButtonDone.active && textButtonDone.visible) {
      textButtonDone.render(guiGraphics, mouseX, mouseY, partialTicks);
    }
  }

  @Override
  public boolean mouseClicked(MouseButtonEvent mouseButtonEvent, boolean doubleClick) {
    if (sliderButton.visible && sliderButton.mouseClicked(mouseButtonEvent, doubleClick)) {
      return true;
    }
    if (textField.visible && textField.mouseClicked(mouseButtonEvent, doubleClick)) {
      textField.setFocused(true);
      return true;
    }

    textButtonDecrease.mouseClicked(mouseButtonEvent, doubleClick);
    textButtonIncrease.mouseClicked(mouseButtonEvent, doubleClick);
    textButtonReset.mouseClicked(mouseButtonEvent, doubleClick);

    if (textButtonEdit.visible) {
      textButtonEdit.mouseClicked(mouseButtonEvent, doubleClick);
    } else if (textButtonDone.visible) {
      textButtonDone.mouseClicked(mouseButtonEvent, doubleClick);
    }
    return super.mouseClicked(mouseButtonEvent, doubleClick);
  }

  @Override
  public boolean mouseReleased(MouseButtonEvent mouseButtonEvent) {
    if (textField.visible) {
      textField.mouseReleased(mouseButtonEvent);
    }
    return super.mouseReleased(mouseButtonEvent);
  }

  @Override
  public boolean mouseScrolled(double x, double y, double scrollDeltaX, double scrollDeltaY) {
    if (sliderButton.visible) {
      return sliderButton.mouseScrolled(x, y, scrollDeltaX, scrollDeltaY);
    } else if (textField.visible) {
      return textField.mouseScrolled(x, y, scrollDeltaX, scrollDeltaY);
    }
    return super.mouseScrolled(x, y, scrollDeltaX, scrollDeltaY);
  }

  @Override
  public void onDrag(MouseButtonEvent mouseButtonEvent, double deltaX, double deltaY) {
    if (sliderButton.visible
        && sliderButton.isMouseOver(mouseButtonEvent.x(), mouseButtonEvent.y())) {
      sliderButton.triggerOnDrag(mouseButtonEvent, deltaX, deltaY);
    }
  }

  @Override
  public boolean charTyped(CharacterEvent characterEvent) {
    if (sliderButton.visible) {
      return sliderButton.charTyped(characterEvent);
    } else if (textField.visible) {
      return textField.charTyped(characterEvent);
    }
    return false;
  }

  @Override
  public boolean keyPressed(KeyEvent keyEvent) {
    if (sliderButton.visible) {
      return sliderButton.keyPressed(keyEvent);
    } else if (textField.visible) {
      return textField.keyPressed(keyEvent);
    }
    return false;
  }

  @Override
  protected void updateWidgetNarration(NarrationElementOutput narrationElementOutput) {
    narrationElementOutput.add(NarratedElementType.TITLE, this.createNarrationMessage());
    if (this.active) {
      String usageKey =
          this.isFocused() ? "narration.slider.usage.focused" : "narration.slider.usage.hovered";
      narrationElementOutput.add(
          NarratedElementType.USAGE, TextComponent.getTranslatedTextRaw(usageKey));
    }
  }

  @Override
  public void setY(int y) {
    super.setY(y);
    this.sliderButton.setY(y);
    this.textField.setY(y);
    this.textButtonDecrease.setY(y);
    this.textButtonIncrease.setY(y);
    this.textButtonReset.setY(y);
    this.textButtonEdit.setY(y);
    this.textButtonDone.setY(y);
  }

  @Override
  public void setX(int x) {
    super.setX(x);
    int sliderX = this.showButtons ? x + DECREASE_BUTTON_WIDTH : x;
    this.sliderButton.setX(sliderX);
    this.textField.setX(sliderX);
    this.textButtonDecrease.setX(sliderX - DECREASE_BUTTON_WIDTH);
    this.textButtonIncrease.setX(sliderX + this.sliderButton.getWidth());
    this.textButtonReset.setX(this.textButtonIncrease.getX() + this.textButtonIncrease.getWidth());
    this.textButtonEdit.setX(this.textButtonReset.getX() + this.textButtonReset.getWidth());
    this.textButtonDone.setX(this.textButtonReset.getX() + this.textButtonReset.getWidth());
  }
}

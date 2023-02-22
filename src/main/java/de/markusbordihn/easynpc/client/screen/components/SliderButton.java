/**
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

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import net.minecraft.client.gui.components.AbstractSliderButton;
import net.minecraft.network.chat.Component;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import de.markusbordihn.easynpc.Constants;

public class SliderButton extends AbstractSliderButton {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  protected final SliderButton.OnChange onChange;

  private float initValue;
  private float minValue;
  protected float maxValue;
  private float valueFraction;
  private float targetValue;

  public SliderButton(int x, int y, int width, int height, Component name, float initValue,
      float minValue, float maxValue, SliderButton.OnChange onChange) {
    super(x, y, width, height, name, initValue);
    this.initValue = initValue;
    this.minValue = minValue;
    this.maxValue = maxValue;
    this.valueFraction = maxValue - minValue;
    this.value = (this.initValue - minValue) / this.valueFraction;
    this.onChange = onChange;
    this.updateTargetValue();
    this.updateMessage();
  }

  public void setDefaultValue(float value) {
    this.initValue = value;
    this.value = (this.initValue - minValue) / this.valueFraction;
    this.applyValue();
    this.updateMessage();
  }

  public float getTargetValue() {
    return this.targetValue;
  }

  private void updateTargetValue() {
    this.targetValue =
        (float) (Math.round((this.minValue + (this.valueFraction * this.value)) * 100.0) / 100.0);
  }

  @OnlyIn(Dist.CLIENT)
  public interface OnChange {
    void onChange(SliderButton sliderButton);
  }

  @Override
  protected void updateMessage() {
    // Round this.value to 2 decimal places and update the message.
    this.setMessage(Component.literal(this.targetValue + ""));
  }

  @Override
  protected void applyValue() {
    this.updateTargetValue();
    this.onChange.onChange(this);
  }

}

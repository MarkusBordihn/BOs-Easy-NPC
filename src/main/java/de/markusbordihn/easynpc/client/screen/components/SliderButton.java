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

  public enum Type {
    DEGREE, POSITION, SCALE, UNKNOWN
  }

  protected final SliderButton.OnChange onChange;

  private float initValue;
  private float minValue;
  private Type type = Type.UNKNOWN;
  protected float maxValue;
  private float valueFraction;
  private float targetValue;
  private float roundFactor = 100.0f;

  public SliderButton(int x, int y, int width, int height, String name, float initValue, Type type,
      SliderButton.OnChange onChange) {
    this(x, y, width, height, Component.literal(name), initValue, getMinValue(type),
        getMaxValue(type), onChange, type);
  }

  public SliderButton(int x, int y, int width, int height, String name, float initValue,
      float minValue, float maxValue, SliderButton.OnChange onChange, Type type) {
    this(x, y, width, height, Component.literal(name), initValue, minValue, maxValue, onChange,
        type);
  }

  public SliderButton(int x, int y, int width, int height, Component name, float initValue,
      float minValue, float maxValue, SliderButton.OnChange onChange, Type type) {
    super(x, y, width, height, name, initValue);
    this.initValue = initValue;
    this.minValue = minValue;
    this.maxValue = maxValue;
    this.valueFraction = maxValue - minValue;
    this.value = (this.initValue - minValue) / this.valueFraction;
    if ((this.minValue == 0 && this.maxValue == 360)
        || (this.minValue == -180 && this.maxValue == 180)) {
      this.roundFactor = 1.0f;
    }
    this.onChange = onChange;
    this.type = type;
    this.updateTargetValue();
    this.updateMessage();
  }

  public void setDefaultValue(float value) {
    this.initValue = value;
    this.value = (this.initValue - minValue) / this.valueFraction;
    this.applyValue();
    this.updateMessage();
  }

  public void reset() {
    this.setDefaultValue(0);
  }

  public void resetToDefault() {
    this.setDefaultValue(this.initValue);
  }

  public float getTargetValue() {
    return this.targetValue;
  }

  private void updateTargetValue() {
    this.targetValue =
        Math.round((this.minValue + (this.valueFraction * this.value)) * roundFactor) / roundFactor;
  }

  @OnlyIn(Dist.CLIENT)
  public interface OnChange {
    void onChange(SliderButton sliderButton);
  }

  @Override
  protected void updateMessage() {
    switch (this.type) {
      case DEGREE:
        this.setMessage(Component.literal(this.targetValue + "°"));
        break;
      case SCALE:
      case POSITION:
        this.setMessage(Component.literal(this.targetValue + ""));
        break;
      default:
        this.setMessage(Component.literal(this.targetValue + ""));
    }
  }

  @Override
  protected void applyValue() {
    this.updateTargetValue();
    this.onChange.onChange(this);
  }

  public static float getMinValue(Type type) {
    switch (type) {
      case DEGREE:
        return -180.0f;
      case SCALE:
        return 0.1f;
      case POSITION:
        return -24.0f;
      default:
        return -100;
    }
  }

  public static float getMaxValue(Type type) {
    switch (type) {
      case DEGREE:
        return 180.0f;
      case SCALE:
        return 10.0f;
      case POSITION:
        return 24.0f;
      default:
        return 100;
    }
  }

}

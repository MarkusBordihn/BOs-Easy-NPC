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

import com.mojang.blaze3d.systems.RenderSystem;
import de.markusbordihn.easynpc.Constants;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.AbstractSliderButton;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.util.Mth;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

public class SliderButton extends AbstractSliderButton {

  public static final int DEFAULT_HEIGHT = 16;
  protected static final Component EMPTY_TEXT = Component.literal("");
  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final ResourceLocation SLIDER_LOCATION =
      new ResourceLocation("textures/gui/slider.png");
  protected final SliderButton.OnChange onChange;
  protected final float maxValue;
  private final float minValue;
  private final float stepSize;
  private final float valueFraction;
  private final Type type;
  private float initValue;
  private float targetValue;
  private float roundFactor = 100.0f;

  public SliderButton(
      int x,
      int y,
      int width,
      String name,
      float initValue,
      Type type,
      SliderButton.OnChange onChange) {
    this(
        x,
        y,
        width,
        DEFAULT_HEIGHT,
        Component.literal(name),
        initValue,
        getMinValue(type),
        getMaxValue(type),
        onChange,
        type);
  }

  public SliderButton(
      int x,
      int y,
      int width,
      int height,
      float initValue,
      Type type,
      SliderButton.OnChange onChange) {
    this(
        x,
        y,
        width,
        height,
        EMPTY_TEXT,
        initValue,
        getMinValue(type),
        getMaxValue(type),
        onChange,
        type);
  }

  public SliderButton(
      int x,
      int y,
      int width,
      int height,
      String name,
      float initValue,
      Type type,
      SliderButton.OnChange onChange) {
    this(
        x,
        y,
        width,
        height,
        Component.literal(name),
        initValue,
        getMinValue(type),
        getMaxValue(type),
        onChange,
        type);
  }

  public SliderButton(
      int x,
      int y,
      int width,
      int height,
      String name,
      double initValue,
      double minValue,
      double maxValue,
      SliderButton.OnChange onChange) {
    this(
        x,
        y,
        width,
        height,
        Component.literal(name),
        (float) initValue,
        (float) minValue,
        (float) maxValue,
        onChange,
        Type.DOUBLE);
  }

  public SliderButton(
      int x,
      int y,
      int width,
      int height,
      Component name,
      float initValue,
      float minValue,
      float maxValue,
      SliderButton.OnChange onChange,
      Type type) {
    super(x, y, width, height, name, initValue);
    this.initValue = initValue;
    this.minValue = minValue;
    this.maxValue = maxValue;
    this.stepSize = getStepSize(type);
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

  private static float getMinValue(Type type) {
    return switch (type) {
      case DEGREE -> -180.0f;
      case DOUBLE -> 0.0f;
      case SCALE -> 0.1f;
      case POSITION -> -24.0f;
      default -> -100;
    };
  }

  private static float getMaxValue(Type type) {
    return switch (type) {
      case DEGREE -> 180.0f;
      case DOUBLE -> 1024f;
      case SCALE -> 10.0f;
      case POSITION -> 24.0f;
      default -> 100;
    };
  }

  private static float getStepSize(Type type) {
    return switch (type) {
      case DEGREE -> 0.5f;
      case DOUBLE -> 1.0f;
      case SCALE -> 0.1f;
      case POSITION -> 0.1f;
      default -> 1.0f;
    };
  }

  public void setDefaultValue(double value) {
    this.setDefaultValue(Math.round(value * roundFactor) / roundFactor);
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

  private void setTargetValue(double value) {
    log.info(
        "setTargetValue: {} ({}) min:{} max:{}", value, this.value, this.minValue, this.maxValue);
    if (value < 0) {
      this.value = 0;
    } else if (value > 1) {
      this.value = 1;
    } else {
      this.value = value;
    }
    this.applyValue();
    this.updateMessage();
  }

  public double getTargetDoubleValue() {
    return this.targetValue;
  }

  private void updateTargetValue() {
    // Round value to round factor.
    this.targetValue =
        Math.round((this.minValue + (this.valueFraction * this.value)) * roundFactor) / roundFactor;
  }

  private double getStepSize() {
    return this.stepSize / this.valueFraction;
  }

  @Override
  protected void updateMessage() {
    switch (this.type) {
      case DEGREE:
        this.setMessage(Component.literal(this.targetValue + "°"));
        break;
      case DOUBLE, SCALE, POSITION:
      default:
        this.setMessage(Component.literal(this.targetValue + ""));
    }
  }

  @Override
  protected void applyValue() {
    this.updateTargetValue();
    this.onChange.onChange(this);
  }

  @Override
  public void renderWidget(GuiGraphics guiGraphics, int mouseX, int mouseY, float partialTicks) {
    this.renderButton(guiGraphics, mouseX, mouseY, partialTicks);
  }

  protected void renderBg(@NotNull GuiGraphics guiGraphics) {
    int i = this.isHoveredOrFocused() ? 60 : 40;

    // Slider: Top Part
    guiGraphics.blit(
        SLIDER_LOCATION,
        this.getX() + (int) (this.value * (this.width - 8)),
        this.getY(),
        0,
        i,
        4,
        this.height);
    guiGraphics.blit(
        SLIDER_LOCATION,
        this.getX() + (int) (this.value * (this.width - 8)) + 4,
        this.getY(),
        196,
        i,
        4,
        this.height);

    // Slider: Bottom Part (last only 4 pixel from the bottom)
    guiGraphics.blit(
        SLIDER_LOCATION,
        this.getX() + (int) (this.value * (this.width - 8)),
        this.getY() + this.height - 4,
        0,
        i + 20 - 4,
        4,
        4);
    guiGraphics.blit(
        SLIDER_LOCATION,
        this.getX() + (int) (this.value * (this.width - 8)) + 4,
        this.getY() + this.height - 4,
        196,
        i + 20 - 4,
        4,
        4);
  }

  @Override
  public boolean keyPressed(int $$0, int $$1, int $$2) {
    log.info("hover: {}, keyPressed: {} {} {}", this.isHoveredOrFocused(), $$0, $$1, $$2);
    boolean $$3 = $$0 == 263;
    if ($$3 || $$0 == 262) {
      float $$4 = $$3 ? -1.0F : 1.0F;
      double incrementalSteps = $$4 * this.getStepSize();
      this.setTargetValue(this.value + incrementalSteps);
    }

    return false;
  }

  @Override
  public boolean mouseScrolled(double x, double y, double distance) {
    log.info("hover: {}, mouseScrolled: {} {} {}", this.isHoveredOrFocused(), x, y, distance);
    if (this.isHoveredOrFocused()) {
      double incrementalSteps = distance * this.getStepSize();
      this.setTargetValue(this.value + incrementalSteps);
    }

    return true;
  }

  public void renderButton(GuiGraphics guiGraphics, int left, int top, float partialTicks) {
    Minecraft minecraft = Minecraft.getInstance();
    Font font = minecraft.font;
    int i = this.isHoveredOrFocused() ? 20 : 0;
    RenderSystem.enableBlend();
    RenderSystem.defaultBlendFunc();
    RenderSystem.enableDepthTest();

    // Top Part
    guiGraphics.blit(SLIDER_LOCATION, this.getX(), this.getY(), 0, i, this.width / 2, this.height);
    guiGraphics.blit(
        SLIDER_LOCATION,
        this.getX() + this.width / 2,
        this.getY(),
        200 - this.width / 2,
        i,
        this.width / 2,
        this.height);

    // Bottom Part (last only 4 pixel from the bottom)
    guiGraphics.blit(
        SLIDER_LOCATION,
        this.getX(),
        this.getY() + this.height - 4,
        0,
        i + 20 - 4,
        this.width / 2,
        4);
    guiGraphics.blit(
        SLIDER_LOCATION,
        this.getX() + this.width / 2,
        this.getY() + this.height - 4,
        200 - this.width / 2,
        i + 20 - 4,
        this.width / 2,
        4);

    this.renderBg(guiGraphics);
    int fgColor = this.active ? Constants.FONT_COLOR_WHITE : Constants.FONT_COLOR_LIGHT_GRAY;
    guiGraphics.drawCenteredString(
        font,
        this.getMessage(),
        this.getX() + this.width / 2,
        this.getY() + (this.height - 8) / 2,
        fgColor | Mth.ceil(this.alpha * 255.0F) << 24);
  }

  public enum Type {
    DOUBLE,
    DEGREE,
    POSITION,
    SCALE,
    UNKNOWN
  }

  @OnlyIn(Dist.CLIENT)
  public interface OnChange {

    void onChange(SliderButton sliderButton);
  }
}

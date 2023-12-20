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
import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.Constants;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.components.AbstractSliderButton;
import net.minecraft.client.renderer.GameRenderer;
import net.minecraft.network.chat.Component;
import net.minecraft.util.Mth;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

public class SliderButton extends AbstractSliderButton {

  public static final int DEFAULT_HEIGHT = 16;
  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  protected final SliderButton.OnChange onChange;
  protected final float maxValue;
  private final float minValue;
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

  public double getTargetDoubleValue() {
    return this.targetValue;
  }

  private void updateTargetValue() {
    this.targetValue =
        Math.round((this.minValue + (this.valueFraction * this.value)) * roundFactor) / roundFactor;
  }

  @Override
  protected void updateMessage() {
    switch (this.type) {
      case DEGREE:
        this.setMessage(Component.literal(this.targetValue + "°"));
        break;
      case DOUBLE:
      case SCALE:
      case POSITION:
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
  protected void renderBg(
      @NotNull PoseStack poseStack, @NotNull Minecraft minecraft, int left, int top) {
    RenderSystem.setShaderTexture(0, WIDGETS_LOCATION);
    RenderSystem.setShaderColor(1.0F, 1.0F, 1.0F, 1.0F);
    int i = (this.isHoveredOrFocused() ? 2 : 1) * 20;

    // Slider: Top Part
    this.blit(
        poseStack,
        this.getX() + (int) (this.value * (this.width - 8)),
        this.getY(),
        0,
        46 + i,
        4,
        this.height);
    this.blit(
        poseStack,
        this.getX() + (int) (this.value * (this.width - 8)) + 4,
        this.getY(),
        196,
        46 + i,
        4,
        this.height);

    // Slider: Bottom Part (last only 4 pixel from the bottom)
    this.blit(
        poseStack,
        this.getX() + (int) (this.value * (this.width - 8)),
        this.getY() + this.height - 4,
        0,
        46 + i + 20 - 4,
        4,
        4);
    this.blit(
        poseStack,
        this.getX() + (int) (this.value * (this.width - 8)) + 4,
        this.getY() + this.height - 4,
        196,
        46 + i + 20 - 4,
        4,
        4);
  }

  @Override
  public void renderButton(PoseStack poseStack, int left, int top, float partialTicks) {
    Minecraft minecraft = Minecraft.getInstance();
    Font font = minecraft.font;
    RenderSystem.setShader(GameRenderer::getPositionTexShader);
    RenderSystem.setShaderTexture(0, WIDGETS_LOCATION);
    RenderSystem.setShaderColor(1.0F, 1.0F, 1.0F, this.alpha);
    int i = this.getYImage(this.isHoveredOrFocused());
    RenderSystem.enableBlend();
    RenderSystem.defaultBlendFunc();
    RenderSystem.enableDepthTest();

    // Top Part
    this.blit(poseStack, this.getX(), this.getY(), 0, 46 + i * 20, this.width / 2, this.height);
    this.blit(
        poseStack,
        this.getX() + this.width / 2,
        this.getY(),
        200 - this.width / 2,
        46 + i * 20,
        this.width / 2,
        this.height);

    // Bottom Part (last only 4 pixel from the bottom)
    this.blit(
        poseStack,
        this.getX(),
        this.getY() + this.height - 4,
        0,
        46 + i * 20 + 20 - 4,
        this.width / 2,
        4);
    this.blit(
        poseStack,
        this.getX() + this.width / 2,
        this.getY() + this.height - 4,
        200 - this.width / 2,
        46 + i * 20 + 20 - 4,
        this.width / 2,
        4);

    this.renderBg(poseStack, minecraft, left, top);
    int fgColor = this.active ? Constants.FONT_COLOR_WHITE : Constants.FONT_COLOR_LIGHT_GRAY;
    drawCenteredString(
        poseStack,
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

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

package de.markusbordihn.easynpc.configui.client.screen.configuration.scaling;

import de.markusbordihn.easynpc.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.client.screen.components.RangeSliderButton;
import de.markusbordihn.easynpc.client.screen.components.SliderButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.configui.client.renderer.screen.EntityConfigScreenRenderer;
import de.markusbordihn.easynpc.configui.client.screen.configuration.ConfigurationScreen;
import de.markusbordihn.easynpc.configui.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.configui.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.data.render.EntityRenderConfig;
import de.markusbordihn.easynpc.data.scale.CustomScale;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class ScalingConfigurationScreen<T extends ConfigurationMenu>
    extends ConfigurationScreen<T> {

  private static final int DIMENSION_UPDATE_TICK = 20;
  protected Button defaultScaleButton;
  protected Checkbox proportionalScalingCheckbox;
  protected RangeSliderButton scaleXSliderButton;
  protected RangeSliderButton scaleYSliderButton;
  protected RangeSliderButton scaleZSliderButton;
  private int dimensionUpdateTicker = 0;
  private boolean isScalingLocked = false;
  private float lastScaleX = 1.0f;
  private float lastScaleY = 1.0f;
  private float lastScaleZ = 1.0f;
  private boolean isUpdatingProportionally = false;

  public ScalingConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  @Override
  public void init() {
    super.init();

    // Default button
    int buttonWidth = 80;
    this.defaultScaleButton =
        this.addRenderableWidget(
            new TextButton(
                this.buttonLeftPos, this.buttonTopPos, buttonWidth, "scaling", button -> {}));
    this.defaultScaleButton.active = false;

    // Basic Position
    int scalePositionLeft = this.contentLeftPos + 165;
    int scalePositionTop = this.contentTopPos + 25;
    int scalePositionSpace = 50;

    // Model Data
    ModelDataCapable<?> modelData = this.getEasyNPC().getEasyNPCModelData();
    CustomScale rootScale = modelData.getModelPartScale(ModelPartType.ROOT);

    // Store initial values
    this.lastScaleX = rootScale.x();
    this.lastScaleY = rootScale.y();
    this.lastScaleZ = rootScale.z();

    // Scale Slider Buttons
    this.scaleXSliderButton =
        this.addRenderableWidget(
            createScaleSlider(
                scalePositionLeft,
                scalePositionTop,
                rootScale.x(),
                slider -> this.updateModelScaleProportional('X')));

    this.scaleYSliderButton =
        this.addRenderableWidget(
            createScaleSlider(
                scalePositionLeft,
                scalePositionTop + scalePositionSpace,
                rootScale.y(),
                slider -> this.updateModelScaleProportional('Y')));

    this.scaleZSliderButton =
        this.addRenderableWidget(
            createScaleSlider(
                scalePositionLeft,
                scalePositionTop + scalePositionSpace * 2,
                rootScale.z(),
                slider -> this.updateModelScaleProportional('Z')));

    // Proportional Scaling Checkbox (positioned under the sliders)
    this.isScalingLocked =
        Math.abs(rootScale.x() - rootScale.y()) <= 0.01f
            && Math.abs(rootScale.x() - rootScale.z()) <= 0.01f
            && Math.abs(rootScale.y() - rootScale.z()) <= 0.01f;
    this.proportionalScalingCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                scalePositionLeft,
                scalePositionTop + scalePositionSpace * 3 + 10,
                "proportional_scaling",
                this.isScalingLocked,
                checkbox -> this.toggleProportionalScaling()));
  }

  private RangeSliderButton createScaleSlider(
      int x, int y, float currentValue, java.util.function.Consumer<SliderButton> onChange) {
    return new RangeSliderButton(
        x, y, 140, 20, currentValue, 1.0F, SliderButton.Type.SCALE, onChange::accept);
  }

  private void drawScaleLabel(GuiGraphics guiGraphics, String key, RangeSliderButton slider) {
    Text.drawConfigString(guiGraphics, this.font, key, slider.getX(), slider.getY() - 10);
  }

  private void toggleProportionalScaling() {
    this.isScalingLocked = this.proportionalScalingCheckbox.selected();
    if (this.isScalingLocked) {
      float currentX = this.scaleXSliderButton.getTargetValue();
      float currentY = this.scaleYSliderButton.getTargetValue();
      float currentZ = this.scaleZSliderButton.getTargetValue();

      if (Math.abs(currentX - currentY) > 0.01f
          || Math.abs(currentX - currentZ) > 0.01f
          || Math.abs(currentY - currentZ) > 0.01f) {
        float averageScale = (currentX + currentY + currentZ) / 3.0f;
        averageScale = (float) Math.ceil(averageScale * 10.0f) / 10.0f;
        this.isUpdatingProportionally = true;
        this.scaleXSliderButton.setTargetValue(averageScale);
        this.scaleYSliderButton.setTargetValue(averageScale);
        this.scaleZSliderButton.setTargetValue(averageScale);
        this.isUpdatingProportionally = false;
        this.updateModelScale();
      }
    }
    this.updateLastScaleValues();
  }

  private void updateModelScaleProportional(char changedAxis) {
    if (this.isUpdatingProportionally) {
      return;
    }

    if (this.isScalingLocked) {
      this.isUpdatingProportionally = true;

      float currentScaleX = this.scaleXSliderButton.getTargetValue();
      float currentScaleY = this.scaleYSliderButton.getTargetValue();
      float currentScaleZ = this.scaleZSliderButton.getTargetValue();

      float scalingRatio = 1.0f;

      // Calculate ratio based on which axis changed
      switch (changedAxis) {
        case 'X':
          scalingRatio = currentScaleX / this.lastScaleX;
          this.scaleYSliderButton.setTargetValue(this.lastScaleY * scalingRatio);
          this.scaleZSliderButton.setTargetValue(this.lastScaleZ * scalingRatio);
          break;
        case 'Y':
          scalingRatio = currentScaleY / this.lastScaleY;
          this.scaleXSliderButton.setTargetValue(this.lastScaleX * scalingRatio);
          this.scaleZSliderButton.setTargetValue(this.lastScaleZ * scalingRatio);
          break;
        case 'Z':
          scalingRatio = currentScaleZ / this.lastScaleZ;
          this.scaleXSliderButton.setTargetValue(this.lastScaleX * scalingRatio);
          this.scaleYSliderButton.setTargetValue(this.lastScaleY * scalingRatio);
          break;
      }

      this.isUpdatingProportionally = false;
    }

    this.updateModelScale();
    this.updateLastScaleValues();
  }

  private void updateModelScale() {
    NetworkMessageHandlerManager.getServerHandler()
        .modelScaleChange(
            this.getEasyNPCUUID(),
            ModelPartType.ROOT,
            new CustomScale(
                this.scaleXSliderButton.getTargetValue(),
                this.scaleYSliderButton.getTargetValue(),
                this.scaleZSliderButton.getTargetValue()));
  }

  private void updateLastScaleValues() {
    this.lastScaleX = this.scaleXSliderButton.getTargetValue();
    this.lastScaleY = this.scaleYSliderButton.getTargetValue();
    this.lastScaleZ = this.scaleZSliderButton.getTargetValue();
  }

  @Override
  public void updateTick() {
    super.updateTick();

    // Force refresh of entity dimensions on the client side.
    if (this.getEasyNPCEntity() != null && this.dimensionUpdateTicker++ > DIMENSION_UPDATE_TICK) {
      this.getEasyNPCEntity().refreshDimensions();
      this.dimensionUpdateTicker = 0;
    }
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);

    ModelDataCapable<?> modelData = this.getEasyNPC().getEasyNPCModelData();
    CustomScale rootScale = modelData.getModelPartScale(ModelPartType.ROOT);
    float yScale = rootScale.y();
    int pixelsPerScale = 31;
    int baselineY = this.contentTopPos + 163;
    float yOffset = -(yScale - 1.0f);

    EntityConfigScreenRenderer.renderEntity(
        guiGraphics,
        this.getEasyNPC(),
        EntityRenderConfig.scaling(this.contentLeftPos + 80, baselineY, pixelsPerScale, yOffset),
        this.xMouse,
        this.yMouse);

    drawScaleLabel(guiGraphics, "scale_x", scaleXSliderButton);
    drawScaleLabel(guiGraphics, "scale_y", scaleYSliderButton);
    drawScaleLabel(guiGraphics, "scale_z", scaleZSliderButton);
  }

  @Override
  protected void renderBg(GuiGraphics guiGraphics, float partialTicks, int mouseX, int mouseY) {
    super.renderBg(guiGraphics, partialTicks, mouseX, mouseY);

    // Entity
    guiGraphics.fill(
        this.contentLeftPos,
        this.contentTopPos,
        this.contentLeftPos + 159,
        this.contentTopPos + 207,
        0xff000000);
    guiGraphics.fill(
        this.contentLeftPos + 1,
        this.contentTopPos + 1,
        this.contentLeftPos + 158,
        this.contentTopPos + 206,
        0xffaaaaaa);

    // Scale lines
    int scaleLinesColor = 0xaa555555;
    int scaleLinesTop = this.contentTopPos + 193;
    int scaleLinesLeft = this.contentLeftPos + 4;
    String[] scaleValues = {"  0", "0.5", "1.0", "1.5", "2.0", "2.5", "3.0"};

    for (String scaleValue : scaleValues) {
      Text.drawString(
          guiGraphics, this.font, scaleValue, scaleLinesLeft, scaleLinesTop - 4, scaleLinesColor);
      guiGraphics.fill(
          this.contentLeftPos + 20,
          scaleLinesTop - 1,
          this.contentLeftPos + 152,
          scaleLinesTop,
          scaleLinesColor);
      scaleLinesTop -= 31;
    }
  }
}

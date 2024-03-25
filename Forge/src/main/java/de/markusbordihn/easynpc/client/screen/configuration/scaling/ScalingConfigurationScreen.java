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

package de.markusbordihn.easynpc.client.screen.configuration.scaling;

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.client.screen.components.SliderButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.client.screen.configuration.ConfigurationScreen;
import de.markusbordihn.easynpc.entity.easynpc.data.ScaleData;
import de.markusbordihn.easynpc.menu.configuration.scaling.ScalingConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandler;
import de.markusbordihn.easynpc.screen.ScreenHelper;
import net.minecraft.client.gui.components.Button;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

@OnlyIn(Dist.CLIENT)
public class ScalingConfigurationScreen extends ConfigurationScreen<ScalingConfigurationMenu> {

  private static final int DIMENSION_UPDATE_TICK = 20;
  private final ScaleData<?> scaleData;
  protected Button defaultScaleButton;
  protected Button defaultScaleXButton;
  protected Button defaultScaleYButton;
  protected Button defaultScaleZButton;
  protected SliderButton scaleXSliderButton;
  protected SliderButton scaleYSliderButton;
  protected SliderButton scaleZSliderButton;
  private int dimensionUpdateTicker = 0;

  public ScalingConfigurationScreen(
      ScalingConfigurationMenu menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
    this.scaleData = this.easyNPC.getEasyNPCScaleData();
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
    this.inventoryLabelX = 8;
    this.inventoryLabelY = this.imageHeight - 92;

    int scalePositionLeft = this.contentLeftPos + 160;
    int scalePositionTop = this.contentTopPos + 20;
    int scalePositionSpace = 60;
    int scaleWidth = 140;

    // Scale Slider for X
    this.scaleXSliderButton =
        this.addRenderableWidget(
            new SliderButton(
                scalePositionLeft,
                scalePositionTop,
                scaleWidth,
                20,
                this.scaleData.getScaleX(),
                SliderButton.Type.SCALE,
                button -> {
                  float scale = button.getTargetValue();
                  if (this.scaleData.getScaleX() != scale) {
                    NetworkMessageHandler.scaleChange(uuid, "x", button.getTargetValue());
                  }
                  this.defaultScaleXButton.active = scale != this.scaleData.getDefaultScaleX();
                }));
    this.defaultScaleXButton =
        this.addRenderableWidget(
            new TextButton(
                scalePositionLeft,
                scalePositionTop + this.scaleXSliderButton.getHeight(),
                scaleWidth,
                "reset",
                button ->
                    this.scaleXSliderButton.setDefaultValue(this.scaleData.getDefaultScaleX())));
    this.defaultScaleXButton.active =
        !this.scaleData.getScaleX().equals(this.scaleData.getDefaultScaleX());

    // Scale Slider for Y
    this.scaleYSliderButton =
        this.addRenderableWidget(
            new SliderButton(
                scalePositionLeft,
                scalePositionTop + scalePositionSpace,
                scaleWidth,
                20,
                this.scaleData.getScaleY(),
                SliderButton.Type.SCALE,
                button -> {
                  float scale = button.getTargetValue();
                  if (this.scaleData.getScaleY() != scale) {
                    NetworkMessageHandler.scaleChange(uuid, "y", button.getTargetValue());
                  }
                  this.defaultScaleYButton.active = scale != this.scaleData.getDefaultScaleY();
                }));
    this.defaultScaleYButton =
        this.addRenderableWidget(
            new TextButton(
                scalePositionLeft,
                scalePositionTop + this.scaleYSliderButton.getHeight() + scalePositionSpace,
                scaleWidth,
                "reset",
                button ->
                    this.scaleYSliderButton.setDefaultValue(this.scaleData.getDefaultScaleY())));
    this.defaultScaleYButton.active =
        !this.scaleData.getScaleY().equals(this.scaleData.getDefaultScaleY());

    // Scale Slider for Z
    this.scaleZSliderButton =
        this.addRenderableWidget(
            new SliderButton(
                scalePositionLeft,
                scalePositionTop + scalePositionSpace * 2,
                scaleWidth,
                20,
                this.scaleData.getScaleZ(),
                SliderButton.Type.SCALE,
                button -> {
                  float scale = button.getTargetValue();
                  if (this.scaleData.getScaleZ() != scale) {
                    NetworkMessageHandler.scaleChange(uuid, "z", button.getTargetValue());
                  }
                  this.defaultScaleZButton.active = scale != this.scaleData.getDefaultScaleZ();
                }));
    this.defaultScaleZButton =
        this.addRenderableWidget(
            new TextButton(
                scalePositionLeft,
                scalePositionTop + this.scaleZSliderButton.getHeight() + scalePositionSpace * 2,
                scaleWidth,
                "reset",
                button ->
                    this.scaleZSliderButton.setDefaultValue(this.scaleData.getDefaultScaleZ())));
    this.defaultScaleZButton.active =
        !this.scaleData.getScaleZ().equals(this.scaleData.getDefaultScaleZ());
  }

  @Override
  public void containerTick() {
    super.containerTick();

    // Force refresh of entity dimensions on the client side.
    if (this.easyNPC.getEntity() != null && this.dimensionUpdateTicker++ > DIMENSION_UPDATE_TICK) {
      this.easyNPC.getEntity().refreshDimensions();
      this.dimensionUpdateTicker = 0;
    }
  }

  @Override
  public void render(PoseStack poseStack, int x, int y, float partialTicks) {
    super.render(poseStack, x, y, partialTicks);

    // Avatar
    ScreenHelper.renderEntityAvatarForScaling(
        this.contentLeftPos + 70,
        this.contentTopPos + 175,
        30,
        this.contentLeftPos + 140 - this.xMouse,
        this.contentTopPos + 30 - this.yMouse,
        this.easyNPC);

    // Label for Scale X
    Text.drawConfigString(
        poseStack,
        this.font,
        "scale_x",
        this.scaleXSliderButton.getX(),
        this.scaleXSliderButton.getY() - 10);

    // Label for Scale Y
    Text.drawConfigString(
        poseStack,
        this.font,
        "scale_y",
        this.scaleYSliderButton.getX(),
        this.scaleYSliderButton.getY() - 10);

    // Label for Scale Z
    Text.drawConfigString(
        poseStack,
        this.font,
        "scale_z",
        this.scaleZSliderButton.getX(),
        this.scaleZSliderButton.getY() - 10);
  }

  @Override
  protected void renderBg(PoseStack poseStack, float partialTicks, int mouseX, int mouseY) {
    super.renderBg(poseStack, partialTicks, mouseX, mouseY);

    // Entity
    fill(
        poseStack,
        this.contentLeftPos,
        this.contentTopPos,
        this.contentLeftPos + 149,
        this.contentTopPos + 187,
        0xff000000);
    fill(
        poseStack,
        this.contentLeftPos + 1,
        this.contentTopPos + 1,
        this.contentLeftPos + 148,
        this.contentTopPos + 186,
        0xffaaaaaa);

    // Scale lines
    int scaleLinesColor = 0xaa555555;
    Text.drawString(
        poseStack,
        this.font,
        "0",
        this.contentLeftPos + 4,
        this.contentTopPos + 172,
        scaleLinesColor);
    fill(
        poseStack,
        this.contentLeftPos + 20,
        this.contentTopPos + 175,
        this.contentLeftPos + 148,
        this.contentTopPos + 176,
        scaleLinesColor);

    Text.drawString(
        poseStack,
        this.font,
        "0.5",
        this.contentLeftPos + 4,
        this.contentTopPos + 141,
        scaleLinesColor);
    fill(
        poseStack,
        this.contentLeftPos + 20,
        this.contentTopPos + 144,
        this.contentLeftPos + 148,
        this.contentTopPos + 145,
        scaleLinesColor);

    Text.drawString(
        poseStack,
        this.font,
        "1.0",
        this.contentLeftPos + 4,
        this.contentTopPos + 111,
        scaleLinesColor);
    fill(
        poseStack,
        this.contentLeftPos + 20,
        this.contentTopPos + 114,
        this.contentLeftPos + 148,
        this.contentTopPos + 115,
        scaleLinesColor);

    Text.drawString(
        poseStack,
        this.font,
        "1.5",
        this.contentLeftPos + 4,
        this.contentTopPos + 81,
        scaleLinesColor);
    fill(
        poseStack,
        this.contentLeftPos + 20,
        this.contentTopPos + 84,
        this.contentLeftPos + 148,
        this.contentTopPos + 85,
        scaleLinesColor);

    Text.drawString(
        poseStack,
        this.font,
        "2.0",
        this.contentLeftPos + 4,
        this.contentTopPos + 51,
        scaleLinesColor);
    fill(
        poseStack,
        this.contentLeftPos + 20,
        this.contentTopPos + 54,
        this.contentLeftPos + 148,
        this.contentTopPos + 55,
        scaleLinesColor);

    Text.drawString(
        poseStack,
        this.font,
        "2.5",
        this.contentLeftPos + 4,
        this.contentTopPos + 21,
        scaleLinesColor);
    fill(
        poseStack,
        this.contentLeftPos + 20,
        this.contentTopPos + 24,
        this.contentLeftPos + 148,
        this.contentTopPos + 25,
        scaleLinesColor);
  }
}

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

import de.markusbordihn.easynpc.client.screen.components.SliderButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.client.screen.configuration.ConfigurationScreen;
import de.markusbordihn.easynpc.entity.easynpc.data.ScaleData;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.screen.ScreenHelper;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class ScalingConfigurationScreen<T extends ConfigurationMenu>
    extends ConfigurationScreen<T> {

  private static final int DIMENSION_UPDATE_TICK = 20;
  protected Button defaultScaleButton;
  protected Button defaultScaleXButton;
  protected Button defaultScaleYButton;
  protected Button defaultScaleZButton;
  protected SliderButton scaleXSliderButton;
  protected SliderButton scaleYSliderButton;
  protected SliderButton scaleZSliderButton;
  private int dimensionUpdateTicker = 0;

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
    this.inventoryLabelX = 8;
    this.inventoryLabelY = this.imageHeight - 92;

    int scalePositionLeft = this.contentLeftPos + 165;
    int scalePositionTop = this.contentTopPos + 20;
    int scalePositionSpace = 60;
    int scaleWidth = 140;

    // Scale Slider for X
    ScaleData<?> scaleData = this.getEasyNPC().getEasyNPCScaleData();
    this.scaleXSliderButton =
        this.addRenderableWidget(
            new SliderButton(
                scalePositionLeft,
                scalePositionTop,
                scaleWidth,
                20,
                scaleData.getScaleX(),
                SliderButton.Type.SCALE,
                button -> {
                  float scale = button.getTargetValue();
                  if (scaleData.getScaleX() != scale) {
                    NetworkMessageHandlerManager.getServerHandler()
                        .scaleChange(this.getNpcUUID(), "x", button.getTargetValue());
                  }
                  this.defaultScaleXButton.active = scale != scaleData.getDefaultScaleX();
                }));
    this.defaultScaleXButton =
        this.addRenderableWidget(
            new TextButton(
                scalePositionLeft,
                scalePositionTop + this.scaleXSliderButton.getHeight(),
                scaleWidth,
                "reset",
                button -> this.scaleXSliderButton.setDefaultValue(scaleData.getDefaultScaleX())));
    this.defaultScaleXButton.active = !scaleData.getScaleX().equals(scaleData.getDefaultScaleX());

    // Scale Slider for Y
    this.scaleYSliderButton =
        this.addRenderableWidget(
            new SliderButton(
                scalePositionLeft,
                scalePositionTop + scalePositionSpace,
                scaleWidth,
                20,
                scaleData.getScaleY(),
                SliderButton.Type.SCALE,
                button -> {
                  float scale = button.getTargetValue();
                  if (scaleData.getScaleY() != scale) {
                    NetworkMessageHandlerManager.getServerHandler()
                        .scaleChange(this.getNpcUUID(), "y", button.getTargetValue());
                  }
                  this.defaultScaleYButton.active = scale != scaleData.getDefaultScaleY();
                }));
    this.defaultScaleYButton =
        this.addRenderableWidget(
            new TextButton(
                scalePositionLeft,
                scalePositionTop + this.scaleYSliderButton.getHeight() + scalePositionSpace,
                scaleWidth,
                "reset",
                button -> this.scaleYSliderButton.setDefaultValue(scaleData.getDefaultScaleY())));
    this.defaultScaleYButton.active = !scaleData.getScaleY().equals(scaleData.getDefaultScaleY());

    // Scale Slider for Z
    this.scaleZSliderButton =
        this.addRenderableWidget(
            new SliderButton(
                scalePositionLeft,
                scalePositionTop + scalePositionSpace * 2,
                scaleWidth,
                20,
                scaleData.getScaleZ(),
                SliderButton.Type.SCALE,
                button -> {
                  float scale = button.getTargetValue();
                  if (scaleData.getScaleZ() != scale) {
                    NetworkMessageHandlerManager.getServerHandler()
                        .scaleChange(this.getNpcUUID(), "z", button.getTargetValue());
                  }
                  this.defaultScaleZButton.active = scale != scaleData.getDefaultScaleZ();
                }));
    this.defaultScaleZButton =
        this.addRenderableWidget(
            new TextButton(
                scalePositionLeft,
                scalePositionTop + this.scaleZSliderButton.getHeight() + scalePositionSpace * 2,
                scaleWidth,
                "reset",
                button -> this.scaleZSliderButton.setDefaultValue(scaleData.getDefaultScaleZ())));
    this.defaultScaleZButton.active = !scaleData.getScaleZ().equals(scaleData.getDefaultScaleZ());
  }

  @Override
  public void containerTick() {
    super.containerTick();

    // Force refresh of entity dimensions on the client side.
    if (this.getEasyNPCEntity() != null && this.dimensionUpdateTicker++ > DIMENSION_UPDATE_TICK) {
      this.getEasyNPCEntity().refreshDimensions();
      this.dimensionUpdateTicker = 0;
    }
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);

    // Avatar
    ScreenHelper.renderEntityAvatarForScaling(
        this.contentLeftPos + 80,
        this.contentTopPos + 192,
        30,
        this.contentLeftPos + 75 - this.xMouse,
        this.contentTopPos + 120 - this.yMouse,
        this.getEasyNPC());

    // Label for Scale X
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "scale_x",
        this.scaleXSliderButton.getX(),
        this.scaleXSliderButton.getY() - 10);

    // Label for Scale Y
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "scale_y",
        this.scaleYSliderButton.getX(),
        this.scaleYSliderButton.getY() - 10);

    // Label for Scale Z
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "scale_z",
        this.scaleZSliderButton.getX(),
        this.scaleZSliderButton.getY() - 10);
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

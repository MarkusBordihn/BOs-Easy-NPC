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

package de.markusbordihn.easynpc.client.screen.configuration.scaling;

import net.minecraft.client.gui.components.Button;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import com.mojang.blaze3d.vertex.PoseStack;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.ScreenHelper;
import de.markusbordihn.easynpc.client.screen.components.SliderButton;
import de.markusbordihn.easynpc.client.screen.configuration.ConfigurationScreen;
import de.markusbordihn.easynpc.menu.configuration.scaling.ScalingConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkHandler;

@OnlyIn(Dist.CLIENT)
public class ScalingConfigurationScreen extends ConfigurationScreen<ScalingConfigurationMenu> {

  protected SliderButton scaleXSliderButton;
  protected SliderButton scaleYSliderButton;
  protected SliderButton scaleZSliderButton;
  protected Button defaultScaleXButton;
  protected Button defaultScaleYButton;
  protected Button defaultScaleZButton;

  public ScalingConfigurationScreen(ScalingConfigurationMenu menu, Inventory inventory,
      Component component) {
    super(menu, inventory, component);
  }

  @Override
  public void init() {
    super.init();

    // Basic Position
    this.inventoryLabelX = 8;
    this.inventoryLabelY = this.imageHeight - 92;

    int scalePositionLeft = this.contentLeftPos + 160;
    int scalePositionTop = this.contentTopPos + 20;
    int scalePositionSpace = 60;
    int scaleWidth = 110;

    // Scale Slider for X
    this.scaleXSliderButton =
        this.addRenderableWidget(new SliderButton(scalePositionLeft, scalePositionTop, scaleWidth,
            20, Component.literal("Scale X"), this.entity.getScaleX(), 0.1f, 10f, button -> {
              float scale = button.getTargetValue();
              if (entity.getScaleX() != scale) {
                NetworkHandler.scaleChange(uuid, "x", button.getTargetValue());
              }
              this.defaultScaleXButton.active = scale != entity.getDefaultScaleX();
            }));
    this.defaultScaleXButton = this.addRenderableWidget(menuButton(scalePositionLeft,
        scalePositionTop + this.scaleXSliderButton.getHeight(), scaleWidth, "reset", button -> {
          this.scaleXSliderButton.setDefaultValue(entity.getDefaultScaleX());
        }));
    this.defaultScaleXButton.active =
        !this.entity.getScaleX().equals(this.entity.getDefaultScaleX());

    // Scale Slider for Y
    this.scaleYSliderButton = this.addRenderableWidget(
        new SliderButton(scalePositionLeft, scalePositionTop + scalePositionSpace, scaleWidth, 20,
            Component.literal("Scale Y"), this.entity.getScaleY(), 0.1f, 10f, button -> {
              float scale = button.getTargetValue();
              if (entity.getScaleY() != scale) {
                NetworkHandler.scaleChange(uuid, "y", button.getTargetValue());
              }
              this.defaultScaleYButton.active = scale != entity.getDefaultScaleY();
            }));
    this.defaultScaleYButton = this.addRenderableWidget(menuButton(scalePositionLeft,
        scalePositionTop + this.scaleYSliderButton.getHeight() + scalePositionSpace, scaleWidth,
        "reset", button -> {
          this.scaleYSliderButton.setDefaultValue(entity.getDefaultScaleY());
        }));
    this.defaultScaleYButton.active =
        !this.entity.getScaleY().equals(this.entity.getDefaultScaleY());

    // Scale Slider for Z
    this.scaleZSliderButton = this.addRenderableWidget(
        new SliderButton(scalePositionLeft, scalePositionTop + scalePositionSpace * 2, scaleWidth,
            20, Component.literal("Scale Z"), this.entity.getScaleZ(), 0.1f, 10f, button -> {
              float scale = button.getTargetValue();
              if (entity.getScaleZ() != scale) {
                NetworkHandler.scaleChange(uuid, "z", button.getTargetValue());
              }
              this.defaultScaleZButton.active = scale != entity.getDefaultScaleZ();
            }));
    this.defaultScaleZButton = this.addRenderableWidget(menuButton(scalePositionLeft,
        scalePositionTop + this.scaleZSliderButton.getHeight() + scalePositionSpace * 2, scaleWidth,
        "reset", button -> {
          this.scaleZSliderButton.setDefaultValue(entity.getDefaultScaleZ());
        }));
    this.defaultScaleZButton.active =
        !this.entity.getScaleZ().equals(this.entity.getDefaultScaleZ());
  }

  @Override
  public void render(PoseStack poseStack, int x, int y, float partialTicks) {
    super.render(poseStack, x, y, partialTicks);

    // Avatar
    ScreenHelper.renderEntityAvatar(this.contentLeftPos + 70, this.contentTopPos + 175, 30,
        this.contentLeftPos + 140 - this.xMouse, this.contentTopPos + 30 - this.yMouse,
        this.entity);

    // Label for Scale X
    this.font.draw(poseStack, Component.translatable(Constants.TEXT_CONFIG_PREFIX + "scale_x"),
        this.scaleXSliderButton.x, this.scaleXSliderButton.y - 10f, 4210752);

    // Label for Scale Y
    this.font.draw(poseStack, Component.translatable(Constants.TEXT_CONFIG_PREFIX + "scale_y"),
        this.scaleYSliderButton.x, this.scaleYSliderButton.y - 10f, 4210752);

    // Label for Scale Z
    this.font.draw(poseStack, Component.translatable(Constants.TEXT_CONFIG_PREFIX + "scale_z"),
        this.scaleZSliderButton.x, this.scaleZSliderButton.y - 10f, 4210752);
  }

  @Override
  protected void renderBg(PoseStack poseStack, float partialTicks, int mouseX, int mouseY) {
    super.renderBg(poseStack, partialTicks, mouseX, mouseY);

    // Entity
    fill(poseStack, this.contentLeftPos, this.contentTopPos, this.contentLeftPos + 149,
        this.contentTopPos + 187, 0xff000000);
    fill(poseStack, this.contentLeftPos + 1, this.contentTopPos + 1, this.contentLeftPos + 148,
        this.contentTopPos + 186, 0xffaaaaaa);

    // Scale lines
    int scaleLinesColor = 0xaa555555;
    this.font.draw(poseStack, Component.literal("0"), this.contentLeftPos + 4f,
        this.contentTopPos + 172f, scaleLinesColor);
    fill(poseStack, this.contentLeftPos + 20, this.contentTopPos + 175, this.contentLeftPos + 148,
        this.contentTopPos + 176, scaleLinesColor);

    this.font.draw(poseStack, Component.literal("0.5"), this.contentLeftPos + 4f,
        this.contentTopPos + 141f, scaleLinesColor);
    fill(poseStack, this.contentLeftPos + 20, this.contentTopPos + 144, this.contentLeftPos + 148,
        this.contentTopPos + 145, scaleLinesColor);

    this.font.draw(poseStack, Component.literal("1.0"), this.contentLeftPos + 4f,
        this.contentTopPos + 111f, scaleLinesColor);
    fill(poseStack, this.contentLeftPos + 20, this.contentTopPos + 114, this.contentLeftPos + 148,
        this.contentTopPos + 115, scaleLinesColor);

    this.font.draw(poseStack, Component.literal("1.5"), this.contentLeftPos + 4f,
        this.contentTopPos + 81f, scaleLinesColor);
    fill(poseStack, this.contentLeftPos + 20, this.contentTopPos + 84, this.contentLeftPos + 148,
        this.contentTopPos + 85, scaleLinesColor);

    this.font.draw(poseStack, Component.literal("2.0"), this.contentLeftPos + 4f,
        this.contentTopPos + 51f, scaleLinesColor);
    fill(poseStack, this.contentLeftPos + 20, this.contentTopPos + 54, this.contentLeftPos + 148,
        this.contentTopPos + 55, scaleLinesColor);

    this.font.draw(poseStack, Component.literal("2.5"), this.contentLeftPos + 4f,
        this.contentTopPos + 21f, scaleLinesColor);
    fill(poseStack, this.contentLeftPos + 20, this.contentTopPos + 24, this.contentLeftPos + 148,
        this.contentTopPos + 25, scaleLinesColor);
  }

}

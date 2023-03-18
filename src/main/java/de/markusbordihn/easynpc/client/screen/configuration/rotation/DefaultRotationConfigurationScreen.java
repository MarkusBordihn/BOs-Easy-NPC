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

package de.markusbordihn.easynpc.client.screen.configuration.rotation;

import net.minecraft.ChatFormatting;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.Checkbox;
import net.minecraft.client.renderer.GameRenderer;
import net.minecraft.core.Rotations;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.player.Inventory;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import com.mojang.blaze3d.platform.NativeImage;
import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.PoseStack;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.components.SliderButton;
import de.markusbordihn.easynpc.menu.configuration.rotation.DefaultRotationConfigurationMenu;
import de.markusbordihn.easynpc.model.ModelPart;
import de.markusbordihn.easynpc.network.NetworkMessage;

@OnlyIn(Dist.CLIENT)
public class DefaultRotationConfigurationScreen
    extends RotationConfigurationScreen<DefaultRotationConfigurationMenu> {

  // Rotation Coordinates EditBoxes
  protected Button rootRotationXResetButton;
  protected Button rootRotationYResetButton;
  protected Button rootRotationZResetButton;
  protected SliderButton rootRotationXSliderButton;
  protected SliderButton rootRotationYSliderButton;
  protected SliderButton rootRotationZSliderButton;
  protected Checkbox rootRotationCheckbox;

  // Rotations
  protected float rootRotationX = 0f;
  protected float rootRotationY = 0f;
  protected float rootRotationZ = 0f;

  // Background
  NativeImage backgroundImage = null;
  ResourceLocation backgroundImageResourceLocation = null;

  public DefaultRotationConfigurationScreen(DefaultRotationConfigurationMenu menu,
      Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.defaultRotationButton.active = false;

    // Root Rotations
    Rotations rootRotations = this.entity.getModelRootRotation();
    this.rootRotationX = rootRotations.getX();
    this.rootRotationY = rootRotations.getY();
    this.rootRotationZ = rootRotations.getZ();

    // Root Rotation X
    this.rootRotationXSliderButton =
        this.addRenderableWidget(new SliderButton(this.contentLeftPos, this.contentTopPos, 60, 20,
            "rootRotationX", (float) Math.toDegrees(rootRotations.getX()), -180f, 180f, slider -> {
              this.rootRotationX = (float) Math.toRadians(slider.getTargetValue());
              NetworkMessage.rotationChange(uuid, ModelPart.ROOT,
                  new Rotations(this.rootRotationX, this.rootRotationY, this.rootRotationZ));
            }));
    this.rootRotationXResetButton = this.addRenderableWidget(
        menuButton(this.rootRotationXSliderButton.x + this.rootRotationXSliderButton.getWidth(),
            this.contentTopPos, 10, new TextComponent("↺"), button -> {
              this.rootRotationX = 0f;
              this.rootRotationXSliderButton.reset();
            }));

    // Root Rotation Y
    this.rootRotationYSliderButton = this.addRenderableWidget(new SliderButton(
        this.rootRotationXResetButton.x + this.rootRotationXResetButton.getWidth() + 5,
        this.contentTopPos, 60, 20, "rootRotationY", (float) Math.toDegrees(rootRotations.getY()),
        -180f, 180f, slider -> {
          this.rootRotationY = (float) Math.toRadians(slider.getTargetValue());
          NetworkMessage.rotationChange(uuid, ModelPart.ROOT,
              new Rotations(this.rootRotationX, this.rootRotationY, this.rootRotationZ));
        }));
    this.rootRotationYResetButton = this.addRenderableWidget(
        menuButton(this.rootRotationYSliderButton.x + this.rootRotationYSliderButton.getWidth(),
            this.contentTopPos, 10, new TextComponent("↺"), button -> {
              this.rootRotationY = 0f;
              this.rootRotationYSliderButton.reset();
            }));

    // Root Rotation Z
    this.rootRotationZSliderButton = this.addRenderableWidget(new SliderButton(
        this.rootRotationYResetButton.x + this.rootRotationYResetButton.getWidth() + 5,
        this.contentTopPos, 60, 20, "rootRotationZ", (float) Math.toDegrees(rootRotations.getZ()),
        -180f, 180f, slider -> {
          this.rootRotationZ = (float) Math.toRadians(slider.getTargetValue());
          NetworkMessage.rotationChange(uuid, ModelPart.ROOT,
              new Rotations(this.rootRotationX, this.rootRotationY, this.rootRotationZ));
        }));
    this.rootRotationZResetButton = this.addRenderableWidget(
        menuButton(this.rootRotationZSliderButton.x + this.rootRotationZSliderButton.getWidth(),
            this.contentTopPos, 10, new TextComponent("↺"), button -> {
              this.rootRotationZ = 0f;
              this.rootRotationZSliderButton.reset();
            }));

    // Lock Root Rotation Checkbox
    this.rootRotationCheckbox = this.addRenderableWidget(new Checkbox(
        this.rootRotationZResetButton.x + this.rootRotationZResetButton.getWidth() + 5,
        this.contentTopPos, 150, 20,
        new TranslatableComponent(Constants.TEXT_CONFIG_PREFIX + "lock_rotation")
            .withStyle(ChatFormatting.WHITE),
        this.entity.getModelLockRotation()) {
      @Override
      public void onPress() {
        super.onPress();
        NetworkMessage.modelLockRotationChange(uuid, this.selected());
      }
    });
  }

  @Override
  public void render(PoseStack poseStack, int x, int y, float partialTicks) {
    super.render(poseStack, x, y, partialTicks);

    // Rotation Text
    this.font.draw(poseStack, "Rotation X", this.rootRotationXSliderButton.x + 5f,
        this.rootRotationXSliderButton.y + 25f, 4210752);
    this.font.draw(poseStack, "Rotation Y", this.rootRotationYSliderButton.x + 5f,
        this.rootRotationYSliderButton.y + 25f, 4210752);
    this.font.draw(poseStack, "Rotation Z", this.rootRotationZSliderButton.x + 5f,
        this.rootRotationZSliderButton.y + 25f, 4210752);
  }

  @Override
  public void renderBackground(PoseStack poseStack) {
    // Use a more transparent background than the default.
    if (this.minecraft != null && this.minecraft.level != null) {
      this.fillGradient(poseStack, 0, 0, this.width, this.height, 0x55000000, 0x55000000);
      net.minecraftforge.common.MinecraftForge.EVENT_BUS.post(
          new net.minecraftforge.client.event.ScreenEvent.BackgroundDrawnEvent(this, poseStack));
    }
  }

  @Override
  protected void renderBg(PoseStack poseStack, float partialTicks, int mouseX, int mouseY) {
    RenderSystem.setShader(GameRenderer::getPositionTexShader);
    RenderSystem.setShaderColor(1.0F, 1.0F, 1.0F, 1.0F);
    RenderSystem.setShaderTexture(0, Constants.TEXTURE_DEMO_BACKGROUND);

    // Main screen: top left
    this.blit(poseStack, leftPos, topPos, 0, 0, 250, 70);

    // Main screen: top right
    this.blit(poseStack, leftPos + 243, topPos, 195, 0, 57, 70);

    // Main screen: bottom left
    this.blit(poseStack, leftPos, topPos + 22, 0, 105, 250, 70);

    // Main screen: bottom right
    this.blit(poseStack, leftPos + 243, topPos + 22, 195, 105, 57, 70);
  }
}

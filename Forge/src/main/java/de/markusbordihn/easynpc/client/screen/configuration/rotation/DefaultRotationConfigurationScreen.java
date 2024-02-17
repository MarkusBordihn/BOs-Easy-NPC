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

package de.markusbordihn.easynpc.client.screen.configuration.rotation;

import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.client.screen.components.SliderButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.data.model.ModelPart;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.menu.configuration.rotation.DefaultRotationConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandler;
import javax.annotation.Nonnull;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.renderer.GameRenderer;
import net.minecraft.core.Rotations;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import net.minecraftforge.client.event.ScreenEvent.BackgroundRendered;

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

  public DefaultRotationConfigurationScreen(
      DefaultRotationConfigurationMenu menu, Inventory inventory, Component component) {
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
        this.addRenderableWidget(
            new SliderButton(
                this.contentLeftPos,
                this.contentTopPos,
                60,
                "rootRotationX",
                (float) Math.toDegrees(rootRotations.getX()),
                SliderButton.Type.DEGREE,
                slider -> {
                  this.rootRotationX = (float) Math.toRadians(slider.getTargetValue());
                  NetworkMessageHandler.rotationChange(
                      uuid,
                      ModelPart.ROOT,
                      new CustomRotation(
                          this.rootRotationX, this.rootRotationY, this.rootRotationZ));
                }));
    this.rootRotationXResetButton =
        this.addRenderableWidget(
            new TextButton(
                this.rootRotationXSliderButton.getX() + this.rootRotationXSliderButton.getWidth(),
                this.contentTopPos,
                10,
                Component.literal("↺"),
                button -> {
                  this.rootRotationX = 0f;
                  this.rootRotationXSliderButton.reset();
                }));

    // Root Rotation Y
    this.rootRotationYSliderButton =
        this.addRenderableWidget(
            new SliderButton(
                this.rootRotationXResetButton.getX() + this.rootRotationXResetButton.getWidth() + 5,
                this.contentTopPos,
                60,
                "rootRotationY",
                (float) Math.toDegrees(rootRotations.getY()),
                SliderButton.Type.DEGREE,
                slider -> {
                  this.rootRotationY = (float) Math.toRadians(slider.getTargetValue());
                  NetworkMessageHandler.rotationChange(
                      uuid,
                      ModelPart.ROOT,
                      new CustomRotation(
                          this.rootRotationX, this.rootRotationY, this.rootRotationZ));
                }));
    this.rootRotationYResetButton =
        this.addRenderableWidget(
            new TextButton(
                this.rootRotationYSliderButton.getX() + this.rootRotationYSliderButton.getWidth(),
                this.contentTopPos,
                10,
                Component.literal("↺"),
                button -> {
                  this.rootRotationY = 0f;
                  this.rootRotationYSliderButton.reset();
                }));

    // Root Rotation Z
    this.rootRotationZSliderButton =
        this.addRenderableWidget(
            new SliderButton(
                this.rootRotationYResetButton.getX() + this.rootRotationYResetButton.getWidth() + 5,
                this.contentTopPos,
                60,
                "rootRotationZ",
                (float) Math.toDegrees(rootRotations.getZ()),
                SliderButton.Type.DEGREE,
                slider -> {
                  this.rootRotationZ = (float) Math.toRadians(slider.getTargetValue());
                  NetworkMessageHandler.rotationChange(
                      uuid,
                      ModelPart.ROOT,
                      new CustomRotation(
                          this.rootRotationX, this.rootRotationY, this.rootRotationZ));
                }));
    this.rootRotationZResetButton =
        this.addRenderableWidget(
            new TextButton(
                this.rootRotationZSliderButton.getX() + this.rootRotationZSliderButton.getWidth(),
                this.contentTopPos,
                10,
                Component.literal("↺"),
                button -> {
                  this.rootRotationZ = 0f;
                  this.rootRotationZSliderButton.reset();
                }));

    // Lock Root Rotation Checkbox
    this.rootRotationCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.rootRotationZResetButton.getX() + this.rootRotationZResetButton.getWidth() + 5,
                this.contentTopPos + 2,
                "lock_rotation",
                this.entity.getModelLockRotation(),
                checkbox ->
                    NetworkMessageHandler.modelLockRotationChange(uuid, checkbox.selected())));
  }

  @Override
  public void render(PoseStack poseStack, int x, int y, float partialTicks) {
    super.render(poseStack, x, y, partialTicks);

    // Rotation Text
    Text.drawString(
        poseStack,
        this.font,
        "Rotation X",
        this.rootRotationXSliderButton.getX() + 5,
        this.rootRotationXSliderButton.getY() + 25);
    Text.drawString(
        poseStack,
        this.font,
        "Rotation Y",
        this.rootRotationYSliderButton.getX() + 5,
        this.rootRotationYSliderButton.getY() + 25);
    Text.drawString(
        poseStack,
        this.font,
        "Rotation Z",
        this.rootRotationZSliderButton.getX() + 5,
        this.rootRotationZSliderButton.getY() + 25);
  }

  @Override
  public void renderBackground(@Nonnull PoseStack poseStack) {
    // Use a more transparent background than the default.
    if (this.clientLevel != null) {
      this.fillGradient(poseStack, 0, 0, this.width, this.height, 0x55000000, 0x55000000);
      net.minecraftforge.common.MinecraftForge.EVENT_BUS.post(
          new BackgroundRendered(this, poseStack));
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

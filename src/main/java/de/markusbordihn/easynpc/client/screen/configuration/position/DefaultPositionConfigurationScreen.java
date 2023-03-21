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

package de.markusbordihn.easynpc.client.screen.configuration.position;

import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.client.renderer.GameRenderer;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.phys.Vec3;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import com.mojang.blaze3d.platform.NativeImage;
import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.PoseStack;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.menu.configuration.position.DefaultPositionConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessage;

@OnlyIn(Dist.CLIENT)
public class DefaultPositionConfigurationScreen
    extends PositionConfigurationScreen<DefaultPositionConfigurationMenu> {

  // Default Values
  private static final float POSITION_STEPS = 0.5f;

  // Position Coordinates EditBoxes
  protected EditBox positionXBox;
  protected EditBox positionYBox;
  protected EditBox positionZBox;

  // Position Coordinates
  protected double positionX = 0.0D;
  protected double positionY = 0.0D;
  protected double positionZ = 0.0D;

  // Position Coordinates Buttons
  protected Button positionXMinusButton;
  protected Button positionXPlusButton;
  protected Button positionYMinusButton;
  protected Button positionYPlusButton;
  protected Button positionZMinusButton;
  protected Button positionZPlusButton;

  // Background
  NativeImage backgroundImage = null;
  ResourceLocation backgroundImageResourceLocation = null;

  public DefaultPositionConfigurationScreen(DefaultPositionConfigurationMenu menu,
      Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.defaultPositionButton.active = false;

    // Position Coordinates
    Vec3 entityPosition = this.entity.position();
    this.positionX = entityPosition.x;
    this.positionY = entityPosition.y;
    this.positionZ = entityPosition.z;

    // X Position
    this.positionXBox = this.addRenderableWidget(new EditBox(this.font, this.contentLeftPos + 15,
        this.contentTopPos, 60, 20, Component.translatable("Position X")));
    this.positionXBox.setMaxLength(8);
    this.positionXBox.setValue(String.valueOf(this.positionX));
    this.positionXBox.setResponder(consumer -> {
      Double newPositionX = getDoubleValue(this.positionXBox.getValue());
      if (newPositionX != null) {
        this.positionX = newPositionX;
        NetworkMessage.positionChange(this.uuid,
            new Vec3(this.positionX, this.positionY, this.positionZ));
      }
    });
    this.positionXMinusButton = this.addRenderableWidget(menuButton(this.positionXBox.getX() - 15,
        this.positionXBox.getY(), 15, Component.literal("-"), button -> {
          this.positionX -= POSITION_STEPS;
          this.positionXBox.setValue(String.valueOf(this.positionX));
        }));
    this.positionXPlusButton = this
        .addRenderableWidget(menuButton(this.positionXBox.getX() + this.positionXBox.getWidth() + 1,
            this.positionXBox.getY(), 15, Component.literal("+"), button -> {
              this.positionX += POSITION_STEPS;
              this.positionXBox.setValue(String.valueOf(this.positionX));
            }));

    // Y Position
    this.positionYBox = this.addRenderableWidget(new EditBox(this.font, this.contentLeftPos + 111,
        this.contentTopPos, 60, 20, Component.translatable("Position Y")));
    this.positionYBox.setMaxLength(8);
    this.positionYBox.setValue(String.valueOf(this.positionY));
    this.positionYBox.setResponder(consumer -> {
      Double newPositionY = getDoubleValue(this.positionYBox.getValue());
      if (newPositionY != null) {
        this.positionY = newPositionY;
        NetworkMessage.positionChange(this.uuid,
            new Vec3(this.positionX, this.positionY, this.positionZ));
      }
    });
    this.positionYMinusButton = this.addRenderableWidget(menuButton(this.positionYBox.getX() - 15,
        this.positionYBox.getY(), 15, Component.literal("-"), button -> {
          this.positionY -= POSITION_STEPS;
          this.positionYBox.setValue(String.valueOf(this.positionY));
        }));
    this.positionYPlusButton = this
        .addRenderableWidget(menuButton(this.positionYBox.getX() + this.positionYBox.getWidth() + 1,
            this.positionYBox.getY(), 15, Component.literal("+"), button -> {
              this.positionY += POSITION_STEPS;
              this.positionYBox.setValue(String.valueOf(this.positionY));
            }));

    // Z Position
    this.positionZBox = this.addRenderableWidget(new EditBox(this.font, this.contentLeftPos + 207,
        this.contentTopPos, 60, 20, Component.translatable("Position Z")));
    this.positionZBox.setMaxLength(8);
    this.positionZBox.setValue(String.valueOf(this.positionZ));
    this.positionZBox.setResponder(consumer -> {
      Double newPositionZ = getDoubleValue(this.positionZBox.getValue());
      if (newPositionZ != null) {
        this.positionZ = newPositionZ;
        NetworkMessage.positionChange(this.uuid,
            new Vec3(this.positionX, this.positionY, this.positionZ));
      }
    });
    this.positionZMinusButton = this.addRenderableWidget(menuButton(this.positionZBox.getX() - 15,
        this.positionZBox.getY(), 15, Component.literal("-"), button -> {
          this.positionZ -= POSITION_STEPS;
          this.positionZBox.setValue(String.valueOf(this.positionZ));
        }));
    this.positionZPlusButton = this
        .addRenderableWidget(menuButton(this.positionZBox.getX() + this.positionZBox.getWidth() + 1,
            this.positionZBox.getY(), 15, Component.literal("+"), button -> {
              this.positionZ += POSITION_STEPS;
              this.positionZBox.setValue(String.valueOf(this.positionZ));
            }));
  }

  @Override
  public void render(PoseStack poseStack, int x, int y, float partialTicks) {
    super.render(poseStack, x, y, partialTicks);

    // Position Text
    this.font.draw(poseStack, "Position X", this.positionXBox.getX() + 5f,
        this.positionXBox.getY() + 25f, 4210752);
    this.font.draw(poseStack, "Position Y", this.positionYBox.getX() + 5f,
        this.positionYBox.getY() + 25f, 4210752);
    this.font.draw(poseStack, "Position Z", this.positionZBox.getX() + 5f,
        this.positionZBox.getY() + 25f, 4210752);
  }

  @Override
  public void renderBackground(PoseStack poseStack) {
    // Use a more transparent background than the default.
    if (this.minecraft != null && this.minecraft.level != null) {
      fillGradient(poseStack, 0, 0, this.width, this.height, 0x55000000, 0x55000000);
      net.minecraftforge.common.MinecraftForge.EVENT_BUS.post(
          new net.minecraftforge.client.event.ScreenEvent.BackgroundRendered(this, poseStack));
    }
  }

  @Override
  protected void renderBg(PoseStack poseStack, float partialTicks, int mouseX, int mouseY) {
    RenderSystem.setShader(GameRenderer::getPositionTexShader);
    RenderSystem.setShaderColor(1.0F, 1.0F, 1.0F, 1.0F);
    RenderSystem.setShaderTexture(0, Constants.TEXTURE_DEMO_BACKGROUND);

    // Main screen: top left
    blit(poseStack, leftPos, topPos, 0, 0, 250, 70);

    // Main screen: top right
    blit(poseStack, leftPos + 243, topPos, 195, 0, 57, 70);

    // Main screen: bottom left
    blit(poseStack, leftPos, topPos + 22, 0, 105, 250, 70);

    // Main screen: bottom right
    blit(poseStack, leftPos + 243, topPos + 22, 195, 105, 57, 70);
  }
}

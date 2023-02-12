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

package de.markusbordihn.easynpc.client.screen.configuration.skin;

import java.util.UUID;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.PoseStack;

import net.minecraft.ChatFormatting;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.screens.Screen;
import net.minecraft.client.gui.screens.inventory.AbstractContainerScreen;
import net.minecraft.client.renderer.GameRenderer;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.world.entity.player.Inventory;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.entity.EasyNPCEntity;
import de.markusbordihn.easynpc.menu.configuration.skin.SkinConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkHandler;
import de.markusbordihn.easynpc.skin.SkinModel;

@OnlyIn(Dist.CLIENT)
public class SkinConfigurationScreen<T extends SkinConfigurationMenu>
    extends AbstractContainerScreen<T> {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  // NPC Entity
  protected final EasyNPCEntity entity;
  protected final UUID uuid;
  protected final SkinModel skinModel;
  protected final boolean isPlayerSkinModel;

  // Buttons
  protected Button closeButton = null;
  protected Button customSkinButton = null;
  protected Button defaultSkinButton = null;
  protected Button playerSkinButton = null;

  // Internal
  protected float xMouse;
  protected float yMouse;

  public SkinConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
    this.entity = menu.getEntity();
    this.uuid = this.entity.getUUID();
    this.skinModel = this.entity.getSkinModel();
    this.isPlayerSkinModel =
        SkinModel.HUMANOID.equals(this.skinModel) || SkinModel.HUMANOID_SLIM.equals(this.skinModel);
  }

  public void closeScreen() {
    Minecraft minecraft = this.minecraft;
    if (minecraft != null) {
      minecraft.setScreen((Screen) null);
    }
  }

  @Override
  public void init() {
    super.init();

    // Default stats
    this.imageHeight = 240;
    this.imageWidth = 275;

    // Basic Position
    this.titleLabelX = 8;
    this.titleLabelY = 6;
    this.topPos = (this.height - this.imageHeight) / 2;
    this.leftPos = (this.width - this.imageWidth) / 2;

    // Skin Types
    int buttonsTopPos = this.topPos + 15;
    this.defaultSkinButton = this.addRenderableWidget(new Button(this.leftPos + 7, buttonsTopPos,
        80, 20, new TranslatableComponent("Default Skin"), onPress -> {
          NetworkHandler.openDialog(uuid, "DefaultSkinConfiguration");
        }));
    this.playerSkinButton =
        this.addRenderableWidget(new Button(this.leftPos + 7 + this.defaultSkinButton.getWidth(),
            buttonsTopPos, 80, 20, new TranslatableComponent("Player Skin"), onPress -> {
              NetworkHandler.openDialog(uuid, "PlayerSkinConfiguration");
            }));
    this.customSkinButton = this.addRenderableWidget(new Button(
        this.leftPos + 7 + this.defaultSkinButton.getWidth() + this.playerSkinButton.getWidth(),
        buttonsTopPos, 80, 20, new TranslatableComponent("Custom Skin"), onPress -> {
          NetworkHandler.openDialog(uuid, "CustomSkinConfiguration");
        }));

    // Actions
    this.closeButton = this.addRenderableWidget(new Button(this.leftPos + 219, this.topPos + 210,
        50, 20, new TranslatableComponent("Close").withStyle(ChatFormatting.GRAY), onPress -> {
          Minecraft minecraft = this.minecraft;
          if (minecraft == null) {
            return;
          }
          minecraft.setScreen((Screen) null);
        }));

    // Default button stats
    this.customSkinButton.active = true;
    this.defaultSkinButton.active = true;
    this.playerSkinButton.active = true;
  }

  @Override
  public void render(PoseStack poseStack, int x, int y, float partialTicks) {
    this.renderBackground(poseStack);
    super.render(poseStack, x, y, partialTicks);
    this.xMouse = x;
    this.yMouse = y;
  }

  @Override
  protected void renderLabels(PoseStack poseStack, int x, int y) {
    this.font.draw(poseStack, this.title, this.titleLabelX, this.titleLabelY, 4210752);
  }

  @Override
  protected void renderBg(PoseStack poseStack, float partialTicks, int mouseX, int mouseY) {
    RenderSystem.setShader(GameRenderer::getPositionTexShader);
    RenderSystem.setShaderColor(1.0F, 1.0F, 1.0F, 1.0F);
    RenderSystem.setShaderTexture(0, Constants.TEXTURE_DEMO_BACKGROUND);

    // Main screen (+50px in height)
    this.blit(poseStack, leftPos, topPos, 0, 0, 250, 170);
    this.blit(poseStack, leftPos + 243, topPos, 215, 0, 35, 170);

    int expandedHeight = 70;
    this.blit(poseStack, leftPos, topPos + expandedHeight + 5, 0, 5, 250, 170);
    this.blit(poseStack, leftPos + 243, topPos + expandedHeight + 5, 215, 5, 35, 170);
  }

  @Override
  public boolean keyPressed(int keyCode, int unused1, int unused2) {
    if (keyCode != 257 && keyCode != 335 && keyCode != 69) {
      return super.keyPressed(keyCode, unused1, unused2);
    } else if (keyCode == 257 || keyCode == 335) {
      return true;
    } else {
      return true;
    }
  }

}

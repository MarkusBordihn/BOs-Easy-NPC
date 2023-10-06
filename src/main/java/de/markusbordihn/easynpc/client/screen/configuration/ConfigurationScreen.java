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

package de.markusbordihn.easynpc.client.screen.configuration;

import java.util.UUID;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.PoseStack;

import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.ImageButton;
import net.minecraft.client.gui.screens.Screen;
import net.minecraft.client.gui.screens.inventory.AbstractContainerScreen;
import net.minecraft.client.multiplayer.ClientLevel;
import net.minecraft.client.player.LocalPlayer;
import net.minecraft.client.renderer.GameRenderer;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.config.CommonConfig;
import de.markusbordihn.easynpc.data.skin.SkinModel;
import de.markusbordihn.easynpc.entity.EasyNPCEntity;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationType;
import de.markusbordihn.easynpc.network.NetworkMessageHandler;

@OnlyIn(Dist.CLIENT)
public class ConfigurationScreen<T extends ConfigurationMenu> extends AbstractContainerScreen<T> {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  // Config values
  protected static final CommonConfig.Config COMMON = CommonConfig.COMMON;

  // General
  protected final ClientLevel clientLevel;
  protected final LocalPlayer localPlayer;
  protected final Minecraft minecraftInstance;

  // NPC Entity
  protected final EasyNPCEntity entity;
  protected final SkinModel skinModel;
  protected final UUID uuid;

  // Buttons
  protected Button closeButton = null;
  protected Button homeButton = null;

  // Internal
  protected float xMouse;
  protected float yMouse;
  protected int bottomPos;
  protected int buttonLeftPos;
  protected int buttonTopPos;
  protected int contentLeftPos;
  protected int contentTopPos;
  protected int rightPos;

  public ConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
    this.entity = menu.getEntity();
    this.skinModel = this.entity.getSkinModel();
    this.uuid = this.entity.getUUID();
    this.minecraftInstance = Minecraft.getInstance();
    this.localPlayer = this.minecraftInstance != null ? this.minecraftInstance.player : null;
    this.clientLevel = this.minecraftInstance != null ? this.minecraftInstance.level : null;
  }

  public void closeScreen() {
    if (this.minecraftInstance != null) {
      this.minecraftInstance.setScreen((Screen) null);
    }
  }

  protected int fontDraw(PoseStack poseStack, String text, float x, float y) {
    return this.font.draw(poseStack, Component.translatable(Constants.TEXT_CONFIG_PREFIX + text), x,
        y, 4210752);
  }

  protected static Button menuButton(int left, int top, int width, String label,
      Button.OnPress onPress) {
    return menuButton(left, top, width,
        Component.translatable(Constants.TEXT_CONFIG_PREFIX + label), onPress);
  }

  protected static Button menuButton(int left, int top, int width, String label, String data,
      Button.OnPress onPress) {
    return menuButton(left, top, width,
        Component.translatable(Constants.TEXT_CONFIG_PREFIX + label, data), onPress);
  }

  protected static Button menuButton(int left, int top, int width, Component label,
      Button.OnPress onPress) {
    return new Button(left, top, width, 20, label, onPress);
  }

  protected static Button menuButtonSmall(int left, int top, int width, Component label,
      Button.OnPress onPress) {
    return new Button(left, top, width, 16, label, onPress);
  }

  protected static Double getDoubleValue(String value) {
    if (value != null && !value.isEmpty()) {
      try {
        return Double.parseDouble(value);
      } catch (NumberFormatException e) {
        log.error("Failed to parse double value: {}", value);
      }
    }
    return null;
  }

  protected boolean hasPermissions(Boolean enabled, Boolean allowInCreative, int permissionLevel) {
    if (Boolean.FALSE.equals(enabled)) {
      return false;
    } else if (Boolean.TRUE.equals(allowInCreative && this.localPlayer != null)
        && this.localPlayer.isCreative()) {
      return true;
    } else if (this.localPlayer != null && this.localPlayer.hasPermissions(permissionLevel)) {
      return true;
    }
    return false;
  }

  @Override
  public void init() {
    super.init();

    // Default stats
    this.imageHeight = 242;
    this.imageWidth = 300;

    // Core Positions
    this.titleLabelX = 8;
    this.titleLabelY = 7;
    this.topPos = ((this.height - this.imageHeight) / 2) + 2;
    this.leftPos = (this.width - this.imageWidth) / 2;
    this.rightPos = this.leftPos + this.imageWidth;
    this.bottomPos = this.topPos + this.imageHeight;
    this.buttonLeftPos = this.leftPos + 17;
    this.buttonTopPos = this.topPos + 17;
    this.contentLeftPos = this.leftPos + 7;
    this.contentTopPos = this.topPos + 43;

    // Close Button
    this.closeButton = this.addRenderableWidget(new ImageButton(this.rightPos - 22, this.topPos + 6,
        10, 10, 60, 38, Constants.TEXTURE_CONFIGURATION, onPress -> {
          closeScreen();
        }));

    // Home Button
    this.homeButton = this.addRenderableWidget(
        new Button(this.leftPos + 7, this.buttonTopPos, 10, 20, Component.literal("<"), onPress -> {
          NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.MAIN);
        }));
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

    // Main screen: top left
    this.blit(poseStack, leftPos, topPos, 0, 0, 250, 170);

    // Main screen: top right
    this.blit(poseStack, leftPos + 243, topPos, 195, 0, 57, 170);

    // Main screen: bottom left
    this.blit(poseStack, leftPos, topPos + 77, 0, 5, 250, 170);

    // Main screen: bottom right
    this.blit(poseStack, leftPos + 243, topPos + 77, 195, 5, 57, 170);
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

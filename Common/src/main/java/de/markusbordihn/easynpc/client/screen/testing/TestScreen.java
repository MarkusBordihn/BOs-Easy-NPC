/*
 * Copyright 2022 Markus Bordihn
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

package de.markusbordihn.easynpc.client.screen.testing;

import com.mojang.blaze3d.systems.RenderSystem;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.components.CloseButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.data.screen.ScreenContainerData;
import de.markusbordihn.easynpc.menu.testing.TestMenu;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.screens.inventory.AbstractContainerScreen;
import net.minecraft.client.renderer.GameRenderer;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.inventory.AbstractContainerMenu;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class TestScreen<T extends AbstractContainerMenu> extends AbstractContainerScreen<T> {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  protected final Minecraft minecraftInstance;
  private final TestMenu testMenu;
  private final ScreenContainerData containerData;
  protected Button closeButton = null;
  protected float xMouse;
  protected float yMouse;
  protected int bottomPos;
  protected int buttonLeftPos;
  protected int buttonTopPos;
  protected int contentLeftPos;
  protected int contentTopPos;
  protected int rightPos;
  private boolean isInitialized = false;

  public TestScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
    this.testMenu = (TestMenu) menu;
    this.containerData = testMenu.getContainerData();

    // General environment Data
    this.minecraftInstance = Minecraft.getInstance();

    log.info(
        "TestScreen created with menu: {} and component: {} and data: {}",
        menu,
        component,
        testMenu.getContainerData());
  }

  protected void define() {
    log.info(
        "Define Container data with {}, {} and {}",
        containerData.getNpcUUID(),
        containerData.getDialogUUID(),
        containerData.getPageIndex());
  }

  @Override
  protected void containerTick() {
    super.containerTick();
    if (!isInitialized && this.containerData.isSynced()) {
      this.define();
      isInitialized = true;
    }
  }

  @Override
  protected void init() {
    super.init();

    // Default stats
    this.imageHeight = 243;
    this.imageWidth = 318;

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
    this.closeButton =
        this.addRenderableWidget(
            new CloseButton(this.rightPos - 15, this.topPos + 4, onPress -> closeScreen()));

    log.info(
        "TestScreen initialized with {}, {} and {}",
        containerData.getNpcUUID(),
        containerData.getDialogUUID(),
        containerData.getPageIndex());
  }

  public void closeScreen() {
    if (this.minecraftInstance != null) {
      this.minecraftInstance.setScreen(null);
    }
  }

  @Override
  protected void renderBg(GuiGraphics guiGraphics, float partialTicks, int mouseX, int mouseY) {
    // Background
    RenderSystem.setShader(GameRenderer::getPositionTexShader);
    RenderSystem.setShaderColor(1.0F, 1.0F, 1.0F, 1.0F);
    RenderSystem.setShaderTexture(0, Constants.TEXTURE_DEMO_BACKGROUND);
    guiGraphics.blit(Constants.TEXTURE_DEMO_BACKGROUND, leftPos, topPos, 0, 0, 210, 160);
    guiGraphics.blit(Constants.TEXTURE_DEMO_BACKGROUND, leftPos + 153, topPos, 132, 0, 120, 160);
    guiGraphics.blit(Constants.TEXTURE_DEMO_BACKGROUND, leftPos, topPos + 77, 0, 5, 210, 170);
    guiGraphics.blit(
        Constants.TEXTURE_DEMO_BACKGROUND, leftPos + 153, topPos + 77, 132, 5, 120, 170);
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);
    if (!this.isInitialized) {
      return;
    }

    Text.drawString(
        guiGraphics,
        this.font,
        this.containerData.getNpcUUID().toString(),
        this.leftPos + 8,
        this.topPos + 16,
        0xFFFFFF);
  }
}

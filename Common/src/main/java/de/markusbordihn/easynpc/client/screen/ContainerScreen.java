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

package de.markusbordihn.easynpc.client.screen;

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.components.CloseButton;
import de.markusbordihn.easynpc.data.screen.AdditionalScreenData;
import de.markusbordihn.easynpc.data.screen.ScreenData;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.menu.EasyNPCMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.network.message.ServerNetworkMessageHandlerInterface;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.screens.inventory.AbstractContainerScreen;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.lwjgl.glfw.GLFW;

public class ContainerScreen<T extends EasyNPCMenu> extends AbstractContainerScreen<T>
    implements ScreenInterface {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  protected static double formerMouseX = -1;
  protected static double formerMouseY = -1;
  protected final Minecraft minecraftInstance;
  protected final ServerNetworkMessageHandlerInterface networkMessageHandler;
  protected final ScreenData screenData;
  protected final AdditionalScreenData additionalScreenData;
  protected float xMouse;
  protected float yMouse;
  protected int rightPos;
  protected int bottomPos;
  protected boolean renderBackground = true;
  protected boolean showCloseButton = true;
  protected Button closeButton = null;
  protected boolean compactMode = false;

  protected ContainerScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);

    // Get basic screen data
    this.screenData = menu.getScreenData();
    this.additionalScreenData = new AdditionalScreenData(this.screenData.additionalData());

    // Get Minecraft instance and network message handler
    this.minecraftInstance = Minecraft.getInstance();
    this.networkMessageHandler = NetworkMessageHandlerManager.getServerHandler();
  }

  protected static void resetFormerMousePosition() {
    setFormerMousePosition(-1, -1);
  }

  protected static void setFormerMousePosition(double x, double y) {
    formerMouseX = x;
    formerMouseY = y;
  }

  @Override
  public EasyNPC<?> getEasyNPC() {
    return menu.getEasyNPC();
  }

  @Override
  public ScreenData getScreenData() {
    return this.screenData;
  }

  @Override
  public AdditionalScreenData getAdditionalScreenData() {
    return this.additionalScreenData;
  }

  public void closeScreen() {
    if (this.minecraftInstance != null) {
      this.minecraftInstance.setScreen(null);
    }
    this.onClose();
  }

  @Override
  protected void init() {
    super.init();

    // Default stats
    this.imageHeight = 243;
    this.imageWidth = 318;
    this.compactMode = this.height < 260;

    // Basic position
    this.titleLabelX = 7;
    this.titleLabelY = -9;
    this.topPos = (this.height - this.imageHeight) / 2 + (this.compactMode ? 2 : 10);
    this.leftPos = (this.width - this.imageWidth) / 2;
    this.rightPos = this.leftPos + this.imageWidth;
    this.bottomPos = this.topPos + this.imageHeight;

    // Set mouse position to former position, to avoid mouse jumps.
    if (formerMouseX > 0 && formerMouseY > 0) {
      GLFW.glfwSetCursorPos(minecraftInstance.getWindow().getWindow(), formerMouseX, formerMouseY);
      resetFormerMousePosition();
    }

    // Close Button
    if (this.showCloseButton) {
      this.closeButton =
          this.addRenderableWidget(
              new CloseButton(this.rightPos - 15, this.topPos + 4, onPress -> closeScreen()));
    }
  }

  @Override
  public boolean mouseClicked(double mouseX, double mouseY, int button) {
    setFormerMousePosition(
        Minecraft.getInstance().mouseHandler.xpos(), Minecraft.getInstance().mouseHandler.ypos());
    return super.mouseClicked(mouseX, mouseY, button);
  }

  @Override
  public void render(PoseStack poseStack, int x, int y, float partialTicks) {
    if (this.renderBackground) {
      super.renderBackground(poseStack);
    }
    super.render(poseStack, x, y, partialTicks);
    this.xMouse = x;
    this.yMouse = y;
  }

  @Override
  protected void renderBg(PoseStack poseStack, float partialTicks, int mouseX, int mouseY) {
    // Render screen background
    this.renderDefaultScreenBg(poseStack, this.leftPos, this.topPos);

    // Render title background for none compact mode
    if (!this.compactMode) {
      this.renderDefaultTitleBg(poseStack, this.leftPos, this.topPos);
    }
  }

  @Override
  public void onClose() {
    resetFormerMousePosition();
    super.onClose();
  }

  @Override
  public boolean keyPressed(int keyCode, int unused1, int unused2) {
    if (keyCode != 257 && keyCode != 335 && keyCode != 69 && keyCode != 73) {
      return super.keyPressed(keyCode, unused1, unused2);
    }
    return keyCode == 257 || keyCode == 335 || keyCode == 73;
  }
}

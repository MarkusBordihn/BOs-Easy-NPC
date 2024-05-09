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
import de.markusbordihn.easynpc.data.action.ActionEventSet;
import de.markusbordihn.easynpc.data.cache.CacheManager;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataSet;
import de.markusbordihn.easynpc.data.screen.ScreenContainerData;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.menu.EasyNPCMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.network.ServerNetworkMessageHandlerInterface;
import java.util.UUID;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.screens.inventory.AbstractContainerScreen;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.inventory.AbstractContainerMenu;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.lwjgl.glfw.GLFW;

public class EasyNPCScreen<T extends AbstractContainerMenu> extends AbstractContainerScreen<T> {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  protected static double formerMouseX = -1;
  protected static double formerMouseY = -1;
  protected final Minecraft minecraftInstance;
  protected final ServerNetworkMessageHandlerInterface networkMessageHandler;
  protected final ScreenContainerData containerData;
  protected float xMouse;
  protected float yMouse;
  private boolean isInitialized = false;

  protected EasyNPCScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
    if (menu instanceof EasyNPCMenu easyNPCMenu) {
      this.containerData = easyNPCMenu.getContainerData();
    } else {
      log.error("Failed to get container data for menu {}", menu);
      this.containerData = null;
    }
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

  protected void defineScreen() {
    log.info(
        "Define Container data with {}, {} and {}",
        containerData.getNpcUUID(),
        containerData.getDialogUUID(),
        containerData.getPageIndex());
  }

  public void closeScreen() {
    if (this.minecraftInstance != null) {
      this.minecraftInstance.setScreen(null);
    }
    this.onClose();
  }

  public UUID getNpcUUID() {
    return this.containerData.getNpcUUID();
  }

  public UUID getDialogUUID() {
    return this.containerData.getDialogUUID();
  }

  public int getPageIndex() {
    return this.containerData.getPageIndex();
  }

  public EasyNPC<?> getEasyNPC() {
    return LivingEntityManager.getEasyNPCEntityByUUID(this.containerData.getNpcUUID());
  }

  public boolean hasActionEventSet() {
    return CacheManager.hasActionDataSet(this.containerData.getNpcUUID());
  }

  public ActionEventSet getActionEventSet() {
    return CacheManager.getActionDataSet(this.containerData.getNpcUUID());
  }

  public boolean hasDialogDataSet() {
    return CacheManager.hasDialogDataSet(this.containerData.getNpcUUID());
  }

  public DialogDataSet getDialogDataSet() {
    return CacheManager.getDialogDataSet(this.containerData.getNpcUUID());
  }

  public boolean hasObjectiveDataSet() {
    return CacheManager.hasObjectiveDataSet(this.containerData.getNpcUUID());
  }

  public ObjectiveDataSet getObjectiveDataSet() {
    return CacheManager.getObjectiveDataSet(this.containerData.getNpcUUID());
  }

  public boolean isSynced() {
    return this.containerData != null
        && this.containerData.isSynced()
        && this.containerData.getNpcUUID() != null
        && CacheManager.hasCacheData(this.containerData.getNpcUUID());
  }

  public DialogDataEntry getDialogData() {
    if (!this.hasDialogDataSet()) {
      return null;
    }
    UUID dialogUUID = this.containerData.getDialogUUID();
    DialogDataSet dialogDataSet = CacheManager.getDialogDataSet(this.containerData.getNpcUUID());
    if (dialogUUID != null) {
      return dialogDataSet.getDialog(dialogUUID);
    }
    return dialogDataSet.getDefaultDialog();
  }

  @Override
  protected void init() {
    super.init();

    // Default stats
    this.imageHeight = 200;
    this.imageWidth = 280;

    // Basic position
    this.topPos = (this.height - this.imageHeight) / 2;
    this.leftPos = (this.width - this.imageWidth) / 2;

    // Set mouse position to former position, to avoid mouse jumps.
    if (formerMouseX > 0 && formerMouseY > 0) {
      GLFW.glfwSetCursorPos(minecraftInstance.getWindow().getWindow(), formerMouseX, formerMouseY);
      resetFormerMousePosition();
    }
  }

  @Override
  protected void containerTick() {
    super.containerTick();
    if (!isInitialized && this.isSynced()) {
      this.defineScreen();
      isInitialized = true;
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
    super.render(poseStack, x, y, partialTicks);
    this.xMouse = x;
    this.yMouse = y;
  }

  @Override
  protected void renderBg(PoseStack poseStack, float v, int i, int i1) {
    // Empty method to avoid rendering background.
  }

  @Override
  public void onClose() {
    resetFormerMousePosition();
    this.isInitialized = false;
    super.onClose();
  }
}

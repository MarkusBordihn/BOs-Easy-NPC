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

package de.markusbordihn.easynpc.client.screen.configuration.equipment;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.client.screen.components.Graphics;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.client.screen.configuration.ConfigurationContainerScreen;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.screen.ScreenHelper;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.player.Inventory;

public class EquipmentConfigurationContainerScreen<T extends ConfigurationMenu>
    extends ConfigurationContainerScreen<T> {

  // Buttons
  protected Button defaultEquipmentButton;

  public EquipmentConfigurationContainerScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  private void createVisibleEquipmentSlotCheckbox(int left, int top, EquipmentSlot equipmentSlot) {
    ModelData<?> modelData = this.getEasyNPC().getEasyNPCModelData();
    boolean modelEquipmentVisibility = modelData.isModelEquipmentVisible(equipmentSlot);
    this.addRenderableWidget(
        new Checkbox(
            left,
            top,
            "",
            modelEquipmentVisibility,
            checkbox ->
                NetworkMessageHandlerManager.getServerHandler()
                    .modelVisibilityChange(
                        this.getEasyNPCUUID(), equipmentSlot, checkbox.selected())));
  }

  @Override
  public void init() {
    super.init();

    // Default button
    int buttonWidth = 80;
    this.defaultEquipmentButton =
        this.addRenderableWidget(
            new TextButton(
                this.buttonLeftPos, this.buttonTopPos, buttonWidth, "equipment", button -> {}));
    this.defaultEquipmentButton.active = false;

    // Basic Position
    this.inventoryLabelX = 8;
    this.inventoryLabelY = this.imageHeight - 92;

    // Equipment Slots
    int slotPositionTop = this.contentTopPos + 20;
    ModelData<?> modelData = this.getEasyNPC().getEasyNPCModelData();
    if (modelData.canUseArmor()) {
      int equipmentSlotLeft = this.contentLeftPos + 75;
      this.createVisibleEquipmentSlotCheckbox(
          equipmentSlotLeft, slotPositionTop + 2, EquipmentSlot.HEAD);

      this.createVisibleEquipmentSlotCheckbox(
          equipmentSlotLeft, slotPositionTop + 20, EquipmentSlot.CHEST);

      this.createVisibleEquipmentSlotCheckbox(
          equipmentSlotLeft, slotPositionTop + 38, EquipmentSlot.LEGS);

      this.createVisibleEquipmentSlotCheckbox(
          equipmentSlotLeft, slotPositionTop + 55, EquipmentSlot.FEET);
    }
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);

    // Avatar
    ScreenHelper.renderEntity(
        this.contentLeftPos + 138,
        this.contentTopPos + 102,
        35,
        this.contentLeftPos + 138 - this.xMouse,
        this.contentTopPos + 50 - this.yMouse,
        this.getEasyNPCLivingEntity());

    this.renderTooltip(guiGraphics, x, y);
  }

  @Override
  protected void renderBg(GuiGraphics guiGraphics, float partialTicks, int mouseX, int mouseY) {
    super.renderBg(guiGraphics, partialTicks, mouseX, mouseY);

    int slotPositionTop = this.contentTopPos + 20;

    // Armors Slots Left
    ModelData<?> modelData = this.getEasyNPC().getEasyNPCModelData();
    if (modelData == null || modelData.canUseArmor()) {
      Graphics.blit(
          guiGraphics,
          Constants.TEXTURE_INVENTORY,
          this.contentLeftPos + 90,
          slotPositionTop,
          7,
          7,
          18,
          72);
    }

    // Main Hand Slot Left
    if (modelData == null || modelData.canUseMainHand()) {
      Graphics.blit(
          guiGraphics,
          Constants.TEXTURE_INVENTORY,
          this.contentLeftPos + 90,
          slotPositionTop + 75,
          7,
          7,
          18,
          18);
    }

    // Off-Hand Slot Right
    if (modelData == null || modelData.canUseOffHand()) {
      Graphics.blit(
          guiGraphics,
          Constants.TEXTURE_INVENTORY,
          this.contentLeftPos + 170,
          slotPositionTop + 75,
          7,
          7,
          18,
          18);
    }

    // Player Inventory Slots
    Graphics.blit(
        guiGraphics,
        Constants.TEXTURE_INVENTORY,
        this.contentLeftPos + 58,
        slotPositionTop + 105,
        7,
        83,
        162,
        54);

    // Player Hotbar Slots
    Graphics.blit(
        guiGraphics,
        Constants.TEXTURE_INVENTORY,
        this.contentLeftPos + 58,
        slotPositionTop + 165,
        7,
        141,
        162,
        18);

    // Entity
    guiGraphics.fill(
        this.contentLeftPos + 109,
        slotPositionTop - 8,
        this.contentLeftPos + 169,
        slotPositionTop + 102,
        0xff000000);
    guiGraphics.fill(
        this.contentLeftPos + 110,
        slotPositionTop - 7,
        this.contentLeftPos + 168,
        slotPositionTop + 101,
        0xffaaaaaa);
  }
}

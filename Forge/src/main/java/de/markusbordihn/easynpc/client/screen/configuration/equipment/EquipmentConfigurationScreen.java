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
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.client.screen.configuration.ConfigurationScreen;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import de.markusbordihn.easynpc.menu.configuration.equipment.EquipmentConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandler;
import de.markusbordihn.easynpc.screen.ScreenHelper;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.player.Inventory;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

@OnlyIn(Dist.CLIENT)
public class EquipmentConfigurationScreen extends ConfigurationScreen<EquipmentConfigurationMenu> {

  protected final ModelData<?> modelData;
  protected Button defaultEquipmentButton;

  public EquipmentConfigurationScreen(
      EquipmentConfigurationMenu menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
    this.modelData = this.easyNPC.getEasyNPCModelData();
  }

  private void createVisibleEquipmentSlotCheckbox(int left, int top, EquipmentSlot equipmentSlot) {
    boolean modelEquipmentVisibility = this.modelData.isModelEquipmentVisible(equipmentSlot);
    this.addRenderableWidget(
        new Checkbox(
            left,
            top,
            "",
            modelEquipmentVisibility,
            checkbox ->
                NetworkMessageHandler.modelVisibilityChange(
                    this.uuid, equipmentSlot, checkbox.selected())));
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
    if (this.modelData.canUseArmor()) {
      int equipmentSlotLeft = this.contentLeftPos + 75;
      this.createVisibleEquipmentSlotCheckbox(
          equipmentSlotLeft, this.contentTopPos + 2, EquipmentSlot.HEAD);

      this.createVisibleEquipmentSlotCheckbox(
          equipmentSlotLeft, this.contentTopPos + 20, EquipmentSlot.CHEST);

      this.createVisibleEquipmentSlotCheckbox(
          equipmentSlotLeft, this.contentTopPos + 38, EquipmentSlot.LEGS);

      this.createVisibleEquipmentSlotCheckbox(
          equipmentSlotLeft, this.contentTopPos + 55, EquipmentSlot.FEET);
    }
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);

    // Avatar
    ScreenHelper.renderScaledEntityAvatar(
        this.contentLeftPos + 138,
        this.contentTopPos + 82,
        35,
        this.contentLeftPos + 140 - this.xMouse,
        this.contentTopPos + 30 - this.yMouse,
        this.easyNPC);

    this.renderTooltip(guiGraphics, x, y);
  }

  @Override
  protected void renderBg(GuiGraphics guiGraphics, float partialTicks, int mouseX, int mouseY) {
    super.renderBg(guiGraphics, partialTicks, mouseX, mouseY);

    // Armors Slots Left
    if (this.modelData.canUseArmor()) {
      guiGraphics.blit(
          Constants.TEXTURE_INVENTORY, this.contentLeftPos + 90, this.contentTopPos, 7, 7, 18, 72);
    }

    // Main Hand Slot Left
    if (this.modelData.canUseMainHand()) {
      guiGraphics.blit(
          Constants.TEXTURE_INVENTORY,
          this.contentLeftPos + 90,
          this.contentTopPos + 75,
          7,
          7,
          18,
          18);
    }

    // Off Hand Slot Right
    if (this.modelData.canUseOffHand()) {
      guiGraphics.blit(
          Constants.TEXTURE_INVENTORY,
          this.contentLeftPos + 170,
          this.contentTopPos + 75,
          7,
          7,
          18,
          18);
    }

    // Player Inventory Slots
    guiGraphics.blit(
        Constants.TEXTURE_INVENTORY,
        this.contentLeftPos + 58,
        this.contentTopPos + 105,
        7,
        83,
        162,
        54);

    // Player Hotbar Slots
    guiGraphics.blit(
        Constants.TEXTURE_INVENTORY,
        this.contentLeftPos + 58,
        this.contentTopPos + 165,
        7,
        141,
        162,
        18);

    // Entity
    guiGraphics.fill(
        this.contentLeftPos + 109,
        this.contentTopPos - 8,
        this.contentLeftPos + 169,
        this.contentTopPos + 102,
        0xff000000);
    guiGraphics.fill(
        this.contentLeftPos + 110,
        this.contentTopPos - 7,
        this.contentLeftPos + 168,
        this.contentTopPos + 101,
        0xffaaaaaa);
  }
}

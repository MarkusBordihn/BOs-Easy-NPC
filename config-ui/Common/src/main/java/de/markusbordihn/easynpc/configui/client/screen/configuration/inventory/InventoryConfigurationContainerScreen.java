/*
 * Copyright 2026 Markus Bordihn
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

package de.markusbordihn.easynpc.configui.client.screen.configuration.inventory;

import de.markusbordihn.easynpc.client.screen.components.Graphics;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.configui.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.configui.client.screen.components.HelpIcon;
import de.markusbordihn.easynpc.configui.client.screen.configuration.ConfigurationContainerScreen;
import de.markusbordihn.easynpc.configui.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.configui.menu.configuration.inventory.InventoryConfigurationMenu;
import de.markusbordihn.easynpc.configui.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.data.attribute.InventoryAttributeType;
import de.markusbordihn.easynpc.data.attribute.InventoryAttributes;
import de.markusbordihn.easynpc.entity.easynpc.data.InventoryDataCapable;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class InventoryConfigurationContainerScreen<T extends ConfigurationMenu>
    extends ConfigurationContainerScreen<T> {

  protected Button defaultInventoryButton;

  public InventoryConfigurationContainerScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  private InventoryAttributes getInventoryAttributes() {
    InventoryDataCapable<?> inventoryData = this.getEasyNPC().getEasyNPCInventoryData();
    return inventoryData != null
        ? inventoryData.getInventoryAttributes()
        : InventoryAttributes.EMPTY;
  }

  private void createInventoryAttributeCheckbox(
      int left, int top, InventoryAttributeType attributeType, boolean selected) {
    this.addRenderableWidget(
        new Checkbox(
            left,
            top,
            attributeType.getAttributeName(),
            selected,
            checkbox ->
                NetworkMessageHandlerManager.getServerHandler()
                    .inventoryAttributeChange(
                        this.getEasyNPCUUID(), attributeType, checkbox.selected())));
    this.addRenderableWidget(
        new HelpIcon(this.leftPos + 296, top, attributeType.getAttributeName() + ".tooltip"));
  }

  @Override
  public void init() {
    super.init();

    int buttonWidth = 80;
    this.defaultInventoryButton =
        this.addRenderableWidget(
            new TextButton(
                this.buttonLeftPos, this.buttonTopPos, buttonWidth, "inventory", button -> {}));
    this.defaultInventoryButton.active = false;

    this.inventoryLabelX = 8;
    this.inventoryLabelY = this.imageHeight - 92;

    InventoryAttributes inventoryAttributes = this.getInventoryAttributes();
    int checkboxLeft = this.contentLeftPos + 58;
    this.createInventoryAttributeCheckbox(
        checkboxLeft,
        this.contentTopPos + 8,
        InventoryAttributeType.PICK_UP_ITEMS,
        inventoryAttributes.pickUpItems());
    this.createInventoryAttributeCheckbox(
        checkboxLeft,
        this.contentTopPos + 28,
        InventoryAttributeType.DROP_ON_DEATH,
        inventoryAttributes.dropOnDeath());
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);
    this.renderTooltip(guiGraphics, x, y);
  }

  @Override
  protected void renderBg(GuiGraphics guiGraphics, float partialTicks, int mouseX, int mouseY) {
    super.renderBg(guiGraphics, partialTicks, mouseX, mouseY);

    Graphics.blit(
        guiGraphics,
        Constants.TEXTURE_INVENTORY,
        this.leftPos + InventoryConfigurationMenu.NPC_INVENTORY_START_POSITION_X - 1,
        this.topPos + InventoryConfigurationMenu.NPC_INVENTORY_START_POSITION_Y - 1,
        7,
        83,
        162,
        54);

    Graphics.blit(
        guiGraphics,
        Constants.TEXTURE_INVENTORY,
        this.leftPos + InventoryConfigurationMenu.PLAYER_INVENTORY_START_POSITION_X - 1,
        this.topPos + InventoryConfigurationMenu.PLAYER_INVENTORY_START_POSITION_Y - 1,
        7,
        83,
        162,
        54);

    Graphics.blit(
        guiGraphics,
        Constants.TEXTURE_INVENTORY,
        this.leftPos + InventoryConfigurationMenu.PLAYER_INVENTORY_START_POSITION_X - 1,
        this.topPos + InventoryConfigurationMenu.HOTBAR_START_POSITION_Y - 1,
        7,
        141,
        162,
        18);
  }
}

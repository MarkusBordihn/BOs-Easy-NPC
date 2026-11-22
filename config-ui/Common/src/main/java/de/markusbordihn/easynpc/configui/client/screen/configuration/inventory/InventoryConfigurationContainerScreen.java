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

import static de.markusbordihn.easynpc.configui.menu.configuration.inventory.InventoryConfigurationMenu.COLUMNS;
import static de.markusbordihn.easynpc.configui.menu.configuration.inventory.InventoryConfigurationMenu.SLOT_SIZE;

import de.markusbordihn.easynpc.client.screen.components.Graphics;
import de.markusbordihn.easynpc.client.screen.components.SpinButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
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
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.stream.IntStream;
import net.minecraft.client.gui.GuiGraphicsExtractor;
import net.minecraft.client.gui.components.Button;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class InventoryConfigurationContainerScreen<T extends ConfigurationMenu>
    extends ConfigurationContainerScreen<T> {

  private static final int PLAYER_SLOT_COUNT_LEFT = 236;
  private static final int NPC_OWNED_SLOT_COLOR = 0x50FF8000;

  protected Button defaultInventoryButton;
  private SpinButton<Integer> playerSlotCountButton;

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

    Set<Integer> playerSlotCounts = new LinkedHashSet<>();
    IntStream.rangeClosed(
            InventoryAttributes.MIN_PLAYER_SLOT_COUNT, InventoryAttributes.MAX_PLAYER_SLOT_COUNT)
        .forEach(playerSlotCounts::add);
    this.playerSlotCountButton =
        this.addRenderableWidget(
            new SpinButton<>(
                this.leftPos + PLAYER_SLOT_COUNT_LEFT,
                this.topPos + InventoryConfigurationMenu.NPC_INVENTORY_START_POSITION_Y + 12,
                56,
                16,
                playerSlotCounts,
                inventoryAttributes.playerSlotCount(),
                spinButton ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .inventoryPlayerSlotCountChange(this.getEasyNPCUUID(), spinButton.get())));
    this.addRenderableWidget(
        new HelpIcon(
            this.leftPos + 296,
            this.topPos + InventoryConfigurationMenu.NPC_INVENTORY_START_POSITION_Y + 12,
            InventoryAttributeType.PLAYER_SLOT_COUNT.getAttributeName() + ".tooltip"));
  }

  @Override
  public void extractRenderState(
      GuiGraphicsExtractor guiGraphics, int x, int y, float partialTicks) {
    super.extractRenderState(guiGraphics, x, y, partialTicks);
    Text.drawConfigString(
        guiGraphics,
        this.font,
        InventoryAttributeType.PLAYER_SLOT_COUNT.getAttributeName(),
        this.leftPos + PLAYER_SLOT_COUNT_LEFT,
        this.topPos + InventoryConfigurationMenu.NPC_INVENTORY_START_POSITION_Y);
  }

  @Override
  public void extractBackground(
      GuiGraphicsExtractor guiGraphics, int mouseX, int mouseY, float partialTicks) {
    super.extractBackground(guiGraphics, mouseX, mouseY, partialTicks);

    Graphics.blit(
        guiGraphics,
        Constants.TEXTURE_INVENTORY,
        this.leftPos + InventoryConfigurationMenu.NPC_INVENTORY_START_POSITION_X - 1,
        this.topPos + InventoryConfigurationMenu.NPC_INVENTORY_START_POSITION_Y - 1,
        7,
        83,
        162,
        54);
    for (int slotIndex = this.playerSlotCountButton.get();
        slotIndex < InventoryDataCapable.INVENTORY_SIZE;
        slotIndex++) {
      int slotLeft =
          this.leftPos
              + InventoryConfigurationMenu.NPC_INVENTORY_START_POSITION_X
              + slotIndex % COLUMNS * SLOT_SIZE;
      int slotTop =
          this.topPos
              + InventoryConfigurationMenu.NPC_INVENTORY_START_POSITION_Y
              + slotIndex / COLUMNS * SLOT_SIZE;
      guiGraphics.fill(slotLeft, slotTop, slotLeft + 16, slotTop + 16, NPC_OWNED_SLOT_COLOR);
    }

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

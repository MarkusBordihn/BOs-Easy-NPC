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

package de.markusbordihn.easynpc.client.screen.inventory;

import static de.markusbordihn.easynpc.menu.inventory.NPCInventoryMenu.COLUMNS;
import static de.markusbordihn.easynpc.menu.inventory.NPCInventoryMenu.NPC_SLOTS_TOP;
import static de.markusbordihn.easynpc.menu.inventory.NPCInventoryMenu.SLOTS_LEFT;
import static de.markusbordihn.easynpc.menu.inventory.NPCInventoryMenu.SLOT_SIZE;

import de.markusbordihn.easynpc.menu.inventory.NPCInventoryMenu;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.screens.inventory.AbstractContainerScreen;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.player.Inventory;

public class NPCInventoryScreen extends AbstractContainerScreen<NPCInventoryMenu> {

  private static final ResourceLocation CONTAINER_TEXTURE =
      new ResourceLocation("textures/gui/container/generic_54.png");
  private static final int TEXTURE_HEADER_HEIGHT = 17;
  private static final int TEXTURE_PLAYER_INVENTORY_TOP = 126;
  private static final int TEXTURE_PLAYER_INVENTORY_HEIGHT = 96;
  private static final int BACKGROUND_COLOR = 0xFFC6C6C6;

  private final int rowCount;

  public NPCInventoryScreen(NPCInventoryMenu menu, Inventory inventory, Component title) {
    super(menu, inventory, title);
    this.rowCount = NPCInventoryMenu.getRowCount(menu.getPlayerSlotCount());
    this.imageHeight =
        TEXTURE_HEADER_HEIGHT + this.rowCount * SLOT_SIZE + TEXTURE_PLAYER_INVENTORY_HEIGHT;
    this.inventoryLabelY = this.imageHeight - 94;
  }

  @Override
  public void render(GuiGraphics guiGraphics, int mouseX, int mouseY, float partialTick) {
    this.renderBackground(guiGraphics);
    super.render(guiGraphics, mouseX, mouseY, partialTick);
    this.renderTooltip(guiGraphics, mouseX, mouseY);
  }

  @Override
  protected void renderBg(GuiGraphics guiGraphics, float partialTick, int mouseX, int mouseY) {
    int npcSlotsHeight = TEXTURE_HEADER_HEIGHT + this.rowCount * SLOT_SIZE;
    guiGraphics.blit(
        CONTAINER_TEXTURE, this.leftPos, this.topPos, 0, 0, this.imageWidth, npcSlotsHeight);
    guiGraphics.blit(
        CONTAINER_TEXTURE,
        this.leftPos,
        this.topPos + npcSlotsHeight,
        0,
        TEXTURE_PLAYER_INVENTORY_TOP,
        this.imageWidth,
        TEXTURE_PLAYER_INVENTORY_HEIGHT);

    int lastRowSlotCount = this.menu.getPlayerSlotCount() - (this.rowCount - 1) * COLUMNS;
    int slotFrameLeft = this.leftPos + SLOTS_LEFT - 1;
    int lastRowTop = this.topPos + NPC_SLOTS_TOP - 1 + (this.rowCount - 1) * SLOT_SIZE;
    guiGraphics.fill(
        slotFrameLeft + lastRowSlotCount * SLOT_SIZE,
        lastRowTop,
        slotFrameLeft + COLUMNS * SLOT_SIZE,
        lastRowTop + SLOT_SIZE,
        BACKGROUND_COLOR);
  }
}

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

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.components.Graphics;
import de.markusbordihn.easynpc.data.action.ActionEventSet;
import de.markusbordihn.easynpc.data.dialog.DialogButtonEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.data.render.RenderDataEntry;
import de.markusbordihn.easynpc.data.screen.AdditionalScreenDataInterface;
import de.markusbordihn.easynpc.data.screen.ScreenData;
import de.markusbordihn.easynpc.data.skin.SkinModel;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinDataCapable;
import de.markusbordihn.easynpc.menu.ClientMenuManager;
import java.util.UUID;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;

public interface ScreenInterface<D extends AdditionalScreenDataInterface> {

  EasyNPC<?> getEasyNPC();

  ScreenData getScreenData();

  D getAdditionalScreenData();

  default UUID getEasyNPCUUID() {
    return this.getScreenData().uuid();
  }

  default Entity getEasyNPCEntity() {
    return getEasyNPC() != null ? getEasyNPC().getEntity() : null;
  }

  default LivingEntity getEasyNPCLivingEntity() {
    return getEasyNPC() != null ? getEasyNPC().getLivingEntity() : null;
  }

  default OwnerDataCapable<?> getOwnerData() {
    return this.getEasyNPC().getEasyNPCOwnerData();
  }

  default SkinModel getSkinModel() {
    if (getEasyNPCEntity() == null) {
      return null;
    }
    EasyNPC<?> easyNPC = getEasyNPC();
    SkinDataCapable<?> skinData = easyNPC.getEasyNPCSkinData();
    return skinData != null ? skinData.getSkinModel() : null;
  }

  default Component getDialogText() {
    return this.hasDialogData() ? this.getDialogData().getDialogText() : null;
  }

  default UUID getDialogUUID() {
    return this.getScreenData().dialogId();
  }

  default UUID getDialogButtonUUID() {
    return this.getScreenData().dialogButtonId();
  }

  default UUID getActionDataEntryUUID() {
    return this.getScreenData().actionDataEntryId();
  }

  default UUID getConditionDataEntryUUID() {
    return this.getScreenData().conditionDataEntryId();
  }

  default int getPageIndex() {
    return this.getScreenData().pageIndex();
  }

  default ActionEventSet getActionEventSet() {
    return this.getAdditionalScreenData().getActionEventSet();
  }

  default DialogDataSet getDialogDataSet() {
    return this.getAdditionalScreenData().getDialogDataSet();
  }

  default RenderDataEntry getRenderDataEntry() {
    return this.getEasyNPC().getEasyNPCRenderData().getRenderDataEntry();
  }

  default boolean hasDialogData() {
    return this.getDialogData() != null;
  }

  default DialogDataEntry getDialogData() {
    UUID dialogId = this.getScreenData().dialogId();
    if (dialogId == null) {
      return null;
    }
    return this.getDialogData(dialogId);
  }

  default DialogDataEntry getDialogData(UUID dialogUUID) {
    return this.getDialogDataSet().getDialog(dialogUUID);
  }

  default DialogButtonEntry getDialogButtonData() {
    if (!this.hasDialog() || this.getScreenData().dialogButtonId() == null) {
      return null;
    }
    DialogDataEntry dialogData = this.getDialogData();
    if (dialogData == null) {
      return null;
    }
    return dialogData.getDialogButton(this.getScreenData().dialogButtonId());
  }

  default boolean hasDialog() {
    return this.getDialogDataSet().hasDialog();
  }

  default boolean isSwitchingToAnotherEasyNPCScreen(
      net.minecraft.client.gui.screens.Screen newScreen) {
    if (newScreen == this) {
      return true;
    }

    if (newScreen == null) {
      return ClientMenuManager.getScreenData() != null;
    }

    return newScreen instanceof ScreenInterface<?>;
  }

  default void renderDefaultScreenBg(GuiGraphics guiGraphics, int leftPos, int topPos) {
    Graphics.blit(guiGraphics, Constants.TEXTURE_DEMO_BACKGROUND, leftPos, topPos, 0, 0, 220, 160);
    Graphics.blit(
        guiGraphics, Constants.TEXTURE_DEMO_BACKGROUND, leftPos + 213, topPos, 132, 0, 120, 160);
    Graphics.blit(
        guiGraphics, Constants.TEXTURE_DEMO_BACKGROUND, leftPos, topPos + 77, 0, 5, 220, 170);
    Graphics.blit(
        guiGraphics,
        Constants.TEXTURE_DEMO_BACKGROUND,
        leftPos + 213,
        topPos + 77,
        132,
        5,
        120,
        170);
  }

  default void renderDefaultTitleBg(GuiGraphics guiGraphics, int leftPos, int topPos) {
    Graphics.blit(
        guiGraphics, Constants.TEXTURE_DEMO_BACKGROUND, leftPos, topPos - 16, 0, 0, 248, 19);
  }
}

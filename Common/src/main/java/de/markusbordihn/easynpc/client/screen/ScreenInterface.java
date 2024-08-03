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
import de.markusbordihn.easynpc.data.attribute.BaseAttributes;
import de.markusbordihn.easynpc.data.dialog.DialogButtonEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataSet;
import de.markusbordihn.easynpc.data.render.RenderDataSet;
import de.markusbordihn.easynpc.data.screen.AdditionalScreenData;
import de.markusbordihn.easynpc.data.screen.ScreenData;
import de.markusbordihn.easynpc.data.skin.SkinModel;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerData;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinData;
import java.util.UUID;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;

public interface ScreenInterface {

  EasyNPC<?> getEasyNPC();

  ScreenData getScreenData();

  AdditionalScreenData getAdditionalScreenData();

  default UUID getEasyNPCUUID() {
    return this.getScreenData().uuid();
  }

  default Entity getEasyNPCEntity() {
    return getEasyNPC() != null ? getEasyNPC().getEntity() : null;
  }

  default LivingEntity getEasyNPCLivingEntity() {
    return getEasyNPC() != null ? getEasyNPC().getLivingEntity() : null;
  }

  default OwnerData<?> getOwnerData() {
    return this.getEasyNPC().getEasyNPCOwnerData();
  }

  default SkinModel getSkinModel() {
    if (getEasyNPCEntity() == null) {
      return null;
    }
    EasyNPC<?> easyNPC = getEasyNPC();
    SkinData<?> skinData = easyNPC.getEasyNPCSkinData();
    return skinData != null ? skinData.getSkinModel() : null;
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

  default int getPageIndex() {
    return this.getScreenData().pageIndex();
  }

  default ActionEventSet getActionEventSet() {
    return this.getAdditionalScreenData().getActionEventSet();
  }

  default BaseAttributes getBaseAttributes() {
    return this.getAdditionalScreenData().getBaseAttributes();
  }

  default DialogDataSet getDialogDataSet() {
    return this.getAdditionalScreenData().getDialogDataSet();
  }

  default ObjectiveDataSet getObjectiveDataSet() {
    return this.getAdditionalScreenData().getObjectiveDataSet();
  }

  default RenderDataSet getRenderDataSet() {
    return this.getEasyNPC().getEasyNPCRenderData().getRenderDataSet();
  }

  default boolean hasDialogData() {
    return this.getDialogData() != null;
  }

  default DialogDataEntry getDialogData() {
    return this.getScreenData().dialogId() != null
        ? this.getDialogData(this.getScreenData().dialogId())
        : this.getDialogDataSet().getDefaultDialog();
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

  default void renderDefaultScreenBg(GuiGraphics guiGraphics, int leftPos, int topPos) {
    Graphics.blit(guiGraphics, Constants.TEXTURE_DEMO_BACKGROUND, leftPos, topPos, 0, 0, 210, 160);
    Graphics.blit(
        guiGraphics, Constants.TEXTURE_DEMO_BACKGROUND, leftPos + 203, topPos, 132, 0, 120, 160);
    Graphics.blit(
        guiGraphics, Constants.TEXTURE_DEMO_BACKGROUND, leftPos, topPos + 77, 0, 5, 210, 170);
    Graphics.blit(
        guiGraphics,
        Constants.TEXTURE_DEMO_BACKGROUND,
        leftPos + 203,
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

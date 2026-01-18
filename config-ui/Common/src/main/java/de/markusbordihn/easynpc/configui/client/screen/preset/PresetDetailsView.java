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

package de.markusbordihn.easynpc.configui.client.screen.preset;

import de.markusbordihn.easynpc.client.screen.components.DrawBoxWithBorder;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.data.preset.PresetData;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;

public class PresetDetailsView {

  private static final float TEXT_SCALE = 0.8f;

  private PresetDetailsView() {
    // Utility class
  }

  public static void render(
      GuiGraphics guiGraphics,
      Font font,
      EasyNPC<?> npc,
      PresetData presetData,
      int x,
      int y,
      int width,
      int height) {
    DrawBoxWithBorder.draw(guiGraphics, x, y, width, height);
    if (npc == null) {
      Text.drawString(guiGraphics, font, Component.literal("No NPC Data"), x + 5, y + 5, 0x3F3F3F);
      return;
    }

    guiGraphics.pose().pushPose();
    guiGraphics.pose().scale(TEXT_SCALE, TEXT_SCALE, 1.0f);

    int scaledX = (int) ((x + 5) / TEXT_SCALE);
    int scaledY = (int) ((y + 3) / TEXT_SCALE);
    int lineHeight = (int) (10 / TEXT_SCALE);
    int line = 0;

    Text.drawString(
        guiGraphics,
        font,
        Component.literal("Type: " + npc.getEntityTypeId()),
        scaledX,
        scaledY + lineHeight * line++,
        0x3F3F3F);

    if (npc.getEasyNPCNavigationData() != null
        && npc.getEasyNPCNavigationData().hasHomePosition()) {
      var homePos = npc.getEasyNPCNavigationData().getHomePosition();
      Text.drawString(
          guiGraphics,
          font,
          Component.literal(
              "Pos: " + homePos.getX() + ", " + homePos.getY() + ", " + homePos.getZ()),
          scaledX,
          scaledY + lineHeight * line++,
          0x3F3F3F);
    }

    if (npc.getEasyNPCOwnerData() != null && npc.getEasyNPCOwnerData().hasNPCOwner()) {
      Text.drawString(
          guiGraphics,
          font,
          Component.literal("Owner: " + npc.getEasyNPCOwnerData().getNPCOwnerName()),
          scaledX,
          scaledY + lineHeight * line++,
          0x3F3F3F);
    }

    if (npc.getEasyNPCSkinData() != null) {
      Text.drawString(
          guiGraphics,
          font,
          Component.literal("Skin: " + npc.getEasyNPCSkinData().getSkinType()),
          scaledX,
          scaledY + lineHeight * line++,
          0x3F3F3F);
    }

    if (presetData != null && presetData.data() != null) {
      CompoundTag data = presetData.data();

      if (data.contains("ActionData")) {
        CompoundTag actionData = data.getCompound("ActionData");
        if (actionData.contains("ActionEventSet")) {
          CompoundTag actionEventSet = actionData.getCompound("ActionEventSet");
          boolean hasActions = false;
          for (String key : actionEventSet.getAllKeys()) {
            if (actionEventSet.contains(key, 9) && !actionEventSet.getList(key, 10).isEmpty()) {
              hasActions = true;
              break;
            }
          }
          if (hasActions) {
            Text.drawString(
                guiGraphics,
                font,
                Component.literal("✓ Has Actions"),
                scaledX,
                scaledY + lineHeight * line++,
                0x00AA00);
          }
        }
      }

      if (data.contains("DialogData")) {
        CompoundTag dialogData = data.getCompound("DialogData");
        if (dialogData.contains("DialogDataSet")
            && !dialogData.getList("DialogDataSet", 10).isEmpty()) {
          Text.drawString(
              guiGraphics,
              font,
              Component.literal("✓ Has Dialog"),
              scaledX,
              scaledY + lineHeight * line++,
              0x00AA00);
        }
      }

      if (data.contains("Offers")) {
        CompoundTag offers = data.getCompound("Offers");
        if (offers.contains("Recipes")) {
          CompoundTag recipes = offers.getCompound("Recipes");
          if (recipes.contains("Recipes") && !recipes.getList("Recipes", 10).isEmpty()) {
            Text.drawString(
                guiGraphics,
                font,
                Component.literal("✓ Has Trades"),
                scaledX,
                scaledY + lineHeight * line++,
                0x00AA00);
          }
        }
      }
    }

    guiGraphics.pose().popPose();
  }
}

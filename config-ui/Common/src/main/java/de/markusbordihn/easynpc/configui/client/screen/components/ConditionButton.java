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

package de.markusbordihn.easynpc.configui.client.screen.components;

import de.markusbordihn.easynpc.client.screen.components.SpriteButton;
import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.network.components.TextComponent;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button.OnPress;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;

public class ConditionButton extends SpriteButton {

  private static final int DEFAULT_HEIGHT = 16;
  private static final int SPRITE_WIDTH = 12;
  private static final int SPRITE_HEIGHT = 12;
  private static final int ADD_SPRITE_X = 4;
  private static final int ADD_SPRITE_Y = 3;
  private static final int EDIT_SPRITE_X = 3;
  private static final int EDIT_SPRITE_Y = 2;
  private static final int ADD_OFFSET_X = 65;
  private static final int ADD_OFFSET_Y = 104;
  private static final int EDIT_OFFSET_X = 64;
  private static final int EDIT_OFFSET_Y = 79;
  private static final int ICON_OFFSET_X = 64;
  private static final int ICON_OFFSET_Y = 178;
  private static final ResourceLocation TEXTURE = Constants.TEXTURE_CONFIGURATION;

  public ConditionButton(int left, int top, int width, int conditionCount, OnPress onPress) {
    super(
        left,
        top,
        width,
        DEFAULT_HEIGHT,
        getLabel(conditionCount),
        TEXTURE,
        conditionCount > 0 ? EDIT_SPRITE_X : ADD_SPRITE_X,
        conditionCount > 0 ? EDIT_SPRITE_Y : ADD_SPRITE_Y,
        conditionCount > 0 ? EDIT_OFFSET_X : ADD_OFFSET_X,
        conditionCount > 0 ? EDIT_OFFSET_Y : ADD_OFFSET_Y,
        SPRITE_WIDTH,
        SPRITE_HEIGHT,
        onPress);
  }

  private static Component getLabel(int conditionCount) {
    if (conditionCount <= 0) {
      return TextComponent.getTranslatedConfigText("add_condition");
    } else if (conditionCount == 1) {
      return TextComponent.getTranslatedConfigText("edit_condition");
    }
    return TextComponent.getTranslatedConfigText("edit_conditions", String.valueOf(conditionCount));
  }

  public static void renderIndicator(GuiGraphics guiGraphics, int x, int y) {
    guiGraphics.blit(TEXTURE, x, y, ICON_OFFSET_X, ICON_OFFSET_Y, SPRITE_WIDTH, SPRITE_HEIGHT);
  }
}

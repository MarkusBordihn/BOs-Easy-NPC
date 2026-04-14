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

package de.markusbordihn.easynpc.configui.client.screen;

import de.markusbordihn.easynpc.client.screen.components.Graphics;
import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.configui.data.screen.AdditionalScreenData;
import de.markusbordihn.easynpc.configui.menu.ClientConfigUIMenuManager;
import de.markusbordihn.easynpc.data.attribute.BaseAttributes;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataSet;
import de.markusbordihn.easynpc.entity.easynpc.data.ConfigurationDataCapable;
import net.minecraft.client.gui.GuiGraphics;

public interface ScreenInterface
    extends de.markusbordihn.easynpc.client.screen.ScreenInterface<AdditionalScreenData> {

  default BaseAttributes getBaseAttributes() {
    return this.getAdditionalScreenData().getBaseAttributes();
  }

  default ConfigurationDataCapable<?> getConfigurationData() {
    return this.getEasyNPC().getEasyNPCConfigurationData();
  }

  default ObjectiveDataSet getObjectiveDataSet() {
    return this.getAdditionalScreenData().getObjectiveDataSet();
  }

  @Override
  default boolean isSwitchingToAnotherEasyNPCScreen(
      net.minecraft.client.gui.screens.Screen newScreen) {
    if (newScreen == this) {
      return true;
    }

    if (newScreen == null) {
      return ClientConfigUIMenuManager.getScreenData() != null;
    }

    return newScreen instanceof ScreenInterface;
  }

  @Override
  default void renderDefaultScreenBg(
      GuiGraphics guiGraphics, int leftPos, int topPos, boolean compactMode) {
    if (compactMode) {
      Graphics.blit(
          guiGraphics,
          Constants.TEXTURE_CONFIG_SCREEN_BACKGROUND,
          leftPos,
          topPos,
          1,
          17,
          333,
          247,
          512,
          512);
    } else {
      Graphics.blit(
          guiGraphics,
          Constants.TEXTURE_CONFIG_SCREEN_BACKGROUND,
          leftPos,
          topPos - 16,
          1,
          1,
          333,
          263,
          512,
          512);
    }
  }
}

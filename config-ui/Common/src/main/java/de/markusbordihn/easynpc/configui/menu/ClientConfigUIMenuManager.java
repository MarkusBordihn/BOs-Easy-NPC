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

package de.markusbordihn.easynpc.configui.menu;

import de.markusbordihn.easynpc.configui.data.screen.AdditionalScreenData;
import de.markusbordihn.easynpc.data.screen.ScreenData;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;

public class ClientConfigUIMenuManager {

  private static ScreenData screenData;
  private static AdditionalScreenData additionalScreenData;
  private static CompoundTag menuData;
  private static UUID menuId;

  private ClientConfigUIMenuManager() {}

  public static void setMenuData(UUID menuId, CompoundTag menuData) {
    ClientConfigUIMenuManager.clearMenuData();
    ClientConfigUIMenuManager.menuId = menuId;
    ClientConfigUIMenuManager.menuData = menuData;

    if (ScreenData.hasScreenData(menuData)) {
      ClientConfigUIMenuManager.screenData = ScreenData.decode(menuData);
      ClientConfigUIMenuManager.additionalScreenData =
          ClientConfigUIMenuManager.screenData != null
              ? new AdditionalScreenData(ClientConfigUIMenuManager.screenData.additionalData())
              : null;
    }
  }

  public static CompoundTag getMenuData() {
    return ClientConfigUIMenuManager.menuData;
  }

  public static ScreenData getScreenData() {
    return ClientConfigUIMenuManager.screenData;
  }

  public static AdditionalScreenData getAdditionalScreenData() {
    return ClientConfigUIMenuManager.additionalScreenData;
  }

  public static boolean hasAdditionalScreenData() {
    return ClientConfigUIMenuManager.additionalScreenData != null;
  }

  public static UUID getMenuId() {
    return ClientConfigUIMenuManager.menuId;
  }

  public static void clearMenuData() {
    ClientConfigUIMenuManager.menuId = null;
    ClientConfigUIMenuManager.menuData = null;
    ClientConfigUIMenuManager.screenData = null;
    ClientConfigUIMenuManager.additionalScreenData = null;
  }
}

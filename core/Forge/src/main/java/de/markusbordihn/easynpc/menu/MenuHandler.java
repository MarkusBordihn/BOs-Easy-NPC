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

package de.markusbordihn.easynpc.menu;

import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerData;
import de.markusbordihn.easynpc.menu.dialog.DialogMenu;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.inventory.MenuType;

public class MenuHandler implements MenuHandlerInterface {

  public MenuHandler() {
    // Register menu handler
  }

  private static boolean hasPermissions(
      ServerPlayer serverPlayer,
      EasyNPC<?> easyNPC,
      Boolean enabled,
      Boolean allowInCreative,
      int permissionLevel) {
    OwnerData<?> ownerData = easyNPC.getEasyNPCOwnerData();
    if (Boolean.FALSE.equals(enabled) || serverPlayer == null) {
      return false;
    } else if (Boolean.TRUE.equals(allowInCreative) && serverPlayer.isCreative()) {
      return true;
    } else if (!ownerData.hasOwner() || !ownerData.isOwner(serverPlayer)) {
      return false;
    } else return serverPlayer.hasPermissions(permissionLevel);
  }

  @Override
  public MenuType<? extends DialogMenu> getDialogMenuType() {
    return ModMenuTypes.DIALOG_MENU.get();
  }
}

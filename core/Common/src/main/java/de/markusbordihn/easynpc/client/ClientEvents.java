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

package de.markusbordihn.easynpc.client;

import de.markusbordihn.easynpc.client.renderer.manager.EntityTypeManager;
import de.markusbordihn.easynpc.data.dialog.DialogDataManager;
import de.markusbordihn.easynpc.io.DataFileHandler;
import net.minecraft.client.Minecraft;

public class ClientEvents {

  private ClientEvents() {}

  public static void handleClientStartedEvent(Minecraft client) {
    DataFileHandler.registerClientDataFiles();
    EntityTypeManager.register();
  }

  public static void handleWorldUnloadEvent() {
    DialogDataManager.clearDialogDataSets();
    clearTextureCaches();
  }

  private static void clearTextureCaches() {
    try {
      Class.forName("de.markusbordihn.easynpc.client.texture.CustomTextureManager")
          .getMethod("clearTextureCache")
          .invoke(null);
      Class.forName("de.markusbordihn.easynpc.client.texture.RemoteTextureManager")
          .getMethod("clearTextureCache")
          .invoke(null);
      Class.forName("de.markusbordihn.easynpc.client.texture.PlayerTextureManager")
          .getMethod("clearTextureCache")
          .invoke(null);
    } catch (Exception e) {
      // Ignore - texture managers might not be loaded yet
    }
  }
}

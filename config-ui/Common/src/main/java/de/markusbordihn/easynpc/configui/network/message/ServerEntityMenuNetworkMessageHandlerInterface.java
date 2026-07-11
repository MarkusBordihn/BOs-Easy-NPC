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

package de.markusbordihn.easynpc.configui.network.message;

import de.markusbordihn.easynpc.configui.network.NetworkHandlerManager;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeNameMessage;
import de.markusbordihn.easynpc.configui.network.message.server.OpenConfigurationMessage;
import de.markusbordihn.easynpc.configui.network.message.server.OpenMenuMessage;
import de.markusbordihn.easynpc.configui.network.message.server.RemoveNPCMessage;
import de.markusbordihn.easynpc.configui.network.message.server.RespawnNPCMessage;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.data.display.NameVisibilityType;
import java.util.UUID;

public interface ServerEntityMenuNetworkMessageHandlerInterface {

  default void openMenu(UUID uuid, UUID menuId) {
    if (uuid != null && menuId != null) {
      NetworkHandlerManager.sendMessageToServer(new OpenMenuMessage(uuid, menuId));
    }
  }

  default void openConfiguration(UUID uuid, ConfigurationType configurationType, int pageIndex) {
    if (uuid != null && configurationType != null && pageIndex >= 0) {
      NetworkHandlerManager.sendMessageToServer(
          new OpenConfigurationMessage(uuid, configurationType, pageIndex));
    }
  }

  default void openConfiguration(UUID uuid, ConfigurationType configurationType) {
    openConfiguration(uuid, configurationType, 0);
  }

  default void changeName(
      UUID uuid, String name, int color, NameVisibilityType nameVisibilityType) {
    if (uuid != null && name != null) {
      NetworkHandlerManager.sendMessageToServer(
          new ChangeNameMessage(uuid, name, color, nameVisibilityType));
    }
  }

  default void removeNPC(UUID uuid) {
    if (uuid != null) {
      NetworkHandlerManager.sendMessageToServer(new RemoveNPCMessage(uuid));
    }
  }

  default void respawnNPC(UUID uuid) {
    if (uuid != null) {
      NetworkHandlerManager.sendMessageToServer(new RespawnNPCMessage(uuid));
    }
  }
}

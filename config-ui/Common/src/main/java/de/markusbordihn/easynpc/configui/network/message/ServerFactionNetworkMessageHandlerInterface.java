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
import de.markusbordihn.easynpc.configui.network.message.server.ChangeFactionColorMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeFactionMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeFactionRelationMessage;
import de.markusbordihn.easynpc.configui.network.message.server.CreateFactionMessage;
import de.markusbordihn.easynpc.configui.network.message.server.OpenFactionEditorMessage;
import de.markusbordihn.easynpc.configui.network.message.server.OpenFactionsEditorMessage;
import de.markusbordihn.easynpc.configui.network.message.server.RemoveFactionEntryMessage;
import java.util.UUID;
import net.minecraft.world.scores.TeamColor;

public interface ServerFactionNetworkMessageHandlerInterface {

  default void changeFaction(UUID uuid, String factionName) {
    if (uuid != null && factionName != null) {
      NetworkHandlerManager.sendMessageToServer(new ChangeFactionMessage(uuid, factionName));
    }
  }

  default void changeFactionColor(UUID uuid, String factionName, TeamColor color) {
    if (uuid != null && factionName != null && !factionName.isEmpty() && color != null) {
      NetworkHandlerManager.sendMessageToServer(
          new ChangeFactionColorMessage(uuid, factionName, color.getSerializedName()));
    }
  }

  default void changeFactionRelation(
      UUID uuid, String factionName, String targetFactionName, boolean hostile, boolean mutual) {
    if (uuid != null
        && factionName != null
        && !factionName.isEmpty()
        && targetFactionName != null
        && !targetFactionName.isEmpty()) {
      NetworkHandlerManager.sendMessageToServer(
          new ChangeFactionRelationMessage(uuid, factionName, targetFactionName, hostile, mutual));
    }
  }

  default void createFaction(UUID uuid, String factionName) {
    if (uuid != null && factionName != null && !factionName.isEmpty()) {
      NetworkHandlerManager.sendMessageToServer(new CreateFactionMessage(uuid, factionName));
    }
  }

  default void removeFactionEntry(UUID uuid, String factionName) {
    if (uuid != null && factionName != null && !factionName.isEmpty()) {
      NetworkHandlerManager.sendMessageToServer(new RemoveFactionEntryMessage(uuid, factionName));
    }
  }

  default void openFactionEditor(UUID uuid, String factionName) {
    if (uuid != null && factionName != null && !factionName.isEmpty()) {
      NetworkHandlerManager.sendMessageToServer(new OpenFactionEditorMessage(uuid, factionName));
    }
  }

  default void openFactionsEditor(UUID uuid) {
    if (uuid != null) {
      NetworkHandlerManager.sendMessageToServer(new OpenFactionsEditorMessage(uuid));
    }
  }
}

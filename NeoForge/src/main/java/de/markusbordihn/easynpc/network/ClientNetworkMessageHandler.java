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

package de.markusbordihn.easynpc.network;

import de.markusbordihn.easynpc.network.message.ClientNetworkMessageHandlerInterface;
import de.markusbordihn.easynpc.network.message.client.OpenMenuCallbackMessageRecord;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerPlayer;
import net.neoforged.neoforge.network.PacketDistributor;

public class ClientNetworkMessageHandler implements ClientNetworkMessageHandlerInterface {

  public ClientNetworkMessageHandler() {
    // Register server network handler for Server -> Client messages.
  }

  @Override
  public void openMenu(UUID uuid, UUID menuId, ServerPlayer serverPlayer, CompoundTag data) {
    if (uuid != null && menuId != null && serverPlayer != null) {
      PacketDistributor.PLAYER
          .with(serverPlayer)
          .send(new OpenMenuCallbackMessageRecord(uuid, menuId, data));
    }
  }
}

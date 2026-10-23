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
package de.markusbordihn.easynpc.handler;

import de.markusbordihn.easynpc.entity.EasyNPCEntityTypes;
import de.markusbordihn.easynpc.network.NetworkHandlerManager;
import de.markusbordihn.easynpc.network.message.client.ShoulderNPCMessage;
import de.markusbordihn.easynpc.network.syncher.ShoulderNPCClientData;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerPlayer;

// Minecraft 1.21.9+ syncs only the parrot variant of shoulder entities to clients, not their tags.
public class ShoulderNPCSynchronizer {

  private static final Map<UUID, ShoulderNPCMessage> sentShoulderStates = new HashMap<>();

  private ShoulderNPCSynchronizer() {}

  public static void handleServerTick(MinecraftServer minecraftServer) {
    if (minecraftServer == null) {
      return;
    }

    List<ServerPlayer> joinedPlayers = new ArrayList<>();
    List<ServerPlayer> knownPlayers = new ArrayList<>();
    List<ShoulderNPCMessage> changedStates = new ArrayList<>();
    Set<UUID> onlinePlayerUUIDs = new HashSet<>();
    for (ServerPlayer serverPlayer : minecraftServer.getPlayerList().getPlayers()) {
      onlinePlayerUUIDs.add(serverPlayer.getUUID());
      ShoulderNPCMessage shoulderState = createShoulderState(serverPlayer);
      ShoulderNPCMessage sentShoulderState =
          sentShoulderStates.put(serverPlayer.getUUID(), shoulderState);
      if (sentShoulderState == null) {
        joinedPlayers.add(serverPlayer);
      } else {
        knownPlayers.add(serverPlayer);
      }
      if (sentShoulderState == null
          ? shoulderState.hasShoulderNPC()
          : !sentShoulderState.equals(shoulderState)) {
        changedStates.add(shoulderState);
      }
    }

    sentShoulderStates
        .entrySet()
        .removeIf(
            entry -> {
              if (onlinePlayerUUIDs.contains(entry.getKey())) {
                return false;
              }
              if (entry.getValue().hasShoulderNPC()) {
                changedStates.add(ShoulderNPCMessage.empty(entry.getValue().playerId()));
              }
              return true;
            });

    for (ServerPlayer knownPlayer : knownPlayers) {
      for (ShoulderNPCMessage changedState : changedStates) {
        NetworkHandlerManager.sendMessageToPlayer(changedState, knownPlayer);
      }
    }
    for (ServerPlayer joinedPlayer : joinedPlayers) {
      for (ShoulderNPCMessage shoulderState : sentShoulderStates.values()) {
        if (shoulderState.hasShoulderNPC()) {
          NetworkHandlerManager.sendMessageToPlayer(shoulderState, joinedPlayer);
        }
      }
    }
  }

  public static void reset() {
    sentShoulderStates.clear();
  }

  public static ShoulderNPCMessage createShoulderState(ServerPlayer serverPlayer) {
    return new ShoulderNPCMessage(
        serverPlayer.getId(),
        getShoulderNPCTag(serverPlayer, serverPlayer.getShoulderEntityLeft()),
        getShoulderNPCTag(serverPlayer, serverPlayer.getShoulderEntityRight()));
  }

  private static CompoundTag getShoulderNPCTag(ServerPlayer serverPlayer, CompoundTag entityTag) {
    return EasyNPCEntityTypes.isEasyNPCEntityTag(entityTag, serverPlayer.level())
        ? ShoulderNPCClientData.createClientEntityTag(entityTag)
        : new CompoundTag();
  }
}

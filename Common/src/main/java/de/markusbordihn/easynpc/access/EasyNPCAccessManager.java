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

package de.markusbordihn.easynpc.access;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.UUID;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class EasyNPCAccessManager {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private EasyNPCAccessManager() {}

  public static boolean hasAccess(CommandSourceStack context, UUID uuid) {
    if (context == null || uuid == null) {
      return false;
    }

    // Check if server player is available and skip access check if not.
    ServerPlayer serverPlayer;
    try {
      serverPlayer = context.getPlayerOrException();
    } catch (CommandSyntaxException serverPlayerException) {

      // Additional check if entity is available, to avoid exploits.
      Entity entity;
      try {
        entity = context.getEntityOrException();
      } catch (CommandSyntaxException entityException) {
        log.warn(
            "Skipping access check for EasyNPC with UUID {} due to missing player and entity!",
            uuid);
        return true;
      }
      log.error("The entity {} tried to access EasyNPC with UUID {}!", entity, uuid);
      return false;
    }

    return hasAccess(serverPlayer, uuid);
  }

  public static boolean hasAccess(ServerPlayer serverPlayer, UUID uuid) {
    if (serverPlayer == null || uuid == null) {
      return false;
    }

    // Try to get the EasyNPC entity by UUID.
    EasyNPC<?> easyNPC = LivingEntityManager.getEasyNPCEntityByUUID(uuid, serverPlayer);
    if (easyNPC == null) {
      log.error("EasyNPC with UUID {} not found!", uuid);
      return false;
    }

    if (!serverPlayer.isCreative() && !easyNPC.getEasyNPCOwnerData().isOwner(serverPlayer)) {
      log.error(
          "Player {} has no access to EasyNPC with UUID {}!",
          serverPlayer.getName().getString(),
          uuid);
      return false;
    }

    return true;
  }
}

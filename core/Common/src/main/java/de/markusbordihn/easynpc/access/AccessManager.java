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

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.security.SecurityDecision;
import de.markusbordihn.easynpc.security.SecurityManager;
import java.util.UUID;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.server.level.ServerPlayer;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class AccessManager {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private AccessManager() {}

  public static boolean hasAccess(CommandSourceStack context, UUID uuid) {
    SecurityDecision decision = SecurityManager.checkAccess(context, uuid);
    log.debug("[Access {}] EasyNPC UUID {} reason {}", decision.allowed(), uuid, decision.reason());
    return decision.allowed();
  }

  public static boolean hasAccess(ServerPlayer serverPlayer, EasyNPC<?> easyNPC) {
    return SecurityManager.checkAccess(serverPlayer, easyNPC).allowed();
  }

  public static boolean hasAccess(ServerPlayer serverPlayer, UUID uuid) {
    return SecurityManager.checkAccess(serverPlayer, uuid).allowed();
  }

  public static EasyNPC<?> getEasyNPCEntityByUUID(UUID uuid, ServerPlayer serverPlayer) {
    return SecurityManager.getEasyNPCEntityByUUID(uuid, serverPlayer);
  }
}

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

package de.markusbordihn.easynpc.entity;

import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.lang.reflect.Proxy;
import java.util.UUID;
import net.minecraft.SharedConstants;
import net.minecraft.server.Bootstrap;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

class LivingEntityManagerTest {

  @BeforeAll
  static void bootstrapMinecraft() {
    SharedConstants.tryDetectVersion();
    Bootstrap.bootStrap();
  }

  private static EasyNPC<?> createEasyNPC(UUID uuid, boolean clientSide) {
    return (EasyNPC<?>)
        Proxy.newProxyInstance(
            EasyNPC.class.getClassLoader(),
            new Class<?>[] {EasyNPC.class},
            (proxy, method, args) ->
                switch (method.getName()) {
                  case "getEntityUUID" -> uuid;
                  case "isClientSideInstance" -> clientSide;
                  case "equals" -> proxy == args[0];
                  case "hashCode" -> System.identityHashCode(proxy);
                  case "toString" -> (clientSide ? "client" : "server") + " EasyNPC " + uuid;
                  default -> null;
                });
  }

  @Test
  void keepsClientAndServerEntitiesWithSameUuidSeparated() {
    UUID uuid = UUID.randomUUID();
    EasyNPC<?> serverEasyNPC = createEasyNPC(uuid, false);
    EasyNPC<?> clientEasyNPC = createEasyNPC(uuid, true);

    LivingEntityManager.addEasyNPC(serverEasyNPC);
    LivingEntityManager.addEasyNPC(clientEasyNPC);
    try {
      assertSame(serverEasyNPC, LivingEntityManager.getServerEasyNPCEntityByUUID(uuid));
      assertSame(clientEasyNPC, LivingEntityManager.getClientEasyNPCEntityByUUID(uuid));

      LivingEntityManager.removeEasyNPC(serverEasyNPC);

      assertNull(LivingEntityManager.getServerEasyNPCEntityByUUID(uuid));
      assertSame(clientEasyNPC, LivingEntityManager.getClientEasyNPCEntityByUUID(uuid));
    } finally {
      LivingEntityManager.removeEasyNPC(clientEasyNPC);
      LivingEntityManager.removeEasyNPC(serverEasyNPC);
    }
  }
}

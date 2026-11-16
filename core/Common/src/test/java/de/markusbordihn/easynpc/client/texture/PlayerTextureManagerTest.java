package de.markusbordihn.easynpc.client.texture;

import static org.junit.jupiter.api.Assertions.assertSame;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinDataCapable;
import java.lang.reflect.Proxy;
import java.util.UUID;
import net.minecraft.SharedConstants;
import net.minecraft.resources.Identifier;
import net.minecraft.server.Bootstrap;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

class PlayerTextureManagerTest {

  @BeforeAll
  static void bootstrapMinecraft() {
    SharedConstants.tryDetectVersion();
    Bootstrap.bootStrap();
  }

  @Test
  void missingSkinUuidUsesDefaultTexture() {
    Identifier fallback = Identifier.parse("easy_npc:textures/entity/test.png");
    assertSame(
        fallback,
        PlayerTextureManager.getOrCreateTextureWithDefault(skinData(null), fallback));
  }

  @Test
  void blankSkinUuidStillUsesDefaultTexture() {
    Identifier fallback = Identifier.parse("easy_npc:textures/entity/test.png");
    assertSame(
        fallback,
        PlayerTextureManager.getOrCreateTextureWithDefault(skinData(Constants.BLANK_UUID), fallback));
  }

  private static SkinDataCapable<?> skinData(UUID uuid) {
    return (SkinDataCapable<?>)
        Proxy.newProxyInstance(
            SkinDataCapable.class.getClassLoader(),
            new Class<?>[] {SkinDataCapable.class},
            (proxy, method, args) -> {
              if (method.getName().equals("getSkinUUID")) {
                return uuid;
              }
              throw new AssertionError("Texture loading must not access " + method.getName());
            });
  }
}

package de.markusbordihn.easynpc.client.renderer.entity.state;

import java.util.UUID;

public interface EasyNPCRenderStateExtension {
  UUID getEasyNpcUUID();

  void setEasyNpcUUID(UUID uuid);
}
